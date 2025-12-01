## usage: python script.py -i HDR.png -dmc img_000_cropped.png img_045_cropped.png img_090_cropped.png img_135_cropped.png -c coordinates.txt

# this script makes frames for each rotation angle (of the original image) where FOVs/ellipses are colored based on PRC (photoreceptor contrast) and in each FOV (ellipse) we have lines that correspond to AoLP (egocentric).
# The thickness of the lines is based on DoLP. The phi_max values are calculated based on Labhart 1985. It also makes an azimuth/PRC scatterplot and line plots for pairs of ommatidia (including the
# difference between the 2 ommatidia (x: solar azimuth, y:PRC) and line plots for both eyes (PRC values) and for each eye separately (PRC values)
# Don't forget to change the true solar azimuth for the PRC plots in the command !!
import os
import sys
import argparse
import cv2
import numpy as np
import subprocess
import math
import matplotlib.pyplot as plt
import scipy
from scipy import stats
from astropy.units import Quantity
from scipy.interpolate import make_interp_spline
from scipy.stats import circstd, circmean
from multiprocessing import Pool
import statistics
import csv
import ast

parser = argparse.ArgumentParser()
parser.add_argument("-i", "--input", required=True, help="INPUT must be the input sky image (.npy HDR image file). It must be square. (required)")
parser.add_argument("-iv", "--input_viz", required=True, help="INPUT_VIZ must be the input sky image made for visualization purposes(.png HDR image file). It must be square. (required)")
parser.add_argument("-dmc", "--demosaiced", nargs='+', required=True, help="DEMOSAICED must be the 4 input CROPPED demosaiced images (000,045,090,135) (.npy files). (required)")
parser.add_argument("-c", "--coordinates", required=True, help="COORDINATES must be a text file with two columns, tab-separated. Each line contains coordinates (azimuth, elevation) for the FOV of one ommatidium. (required)")
parser.add_argument("-saz", "--solarazimuth", required=True, help="SOLARAZIMUTH must be the true solar azimuth in the image (including magnetic declination). (required)")
parser.add_argument("-t", "--threads", required=True, help="THREADS is the number of threads you want the script to use. (required)")
parser.add_argument("-ps", "--pol_sens", required=True, help="POL_SENS is the polarization sensitivity of the simulated animal. (required)")
parser.add_argument("-sigma", "--sigma", required=True, help="SIGMA is the sigma value (in degrees) for the receptive fields of ommatidia of the simulated animal. It is defined as FWHM/2.355(required)")
args=parser.parse_args()

Sp = float(args.pol_sens) # polarization sensitivity as defined in Labhart, 1980 is 6.6 for honeybees

f1 = open("forfanshapemismatch_1steye.tsv", "a", newline="") # for storing data regarding fan-shape-AoLP mismatch
f2 = open("forfanshapemismatch_2ndeye.tsv", "a", newline="") # for storing data regarding fan-shape-AoLP mismatch
f3 = open("forfanshapemismatch_botheyes.tsv", "a", newline="") # for storing data regarding fan-shape-AoLP mismatch
writer1 = csv.writer(f1, delimiter="\t")
writer2 = csv.writer(f2, delimiter="\t")
writer3 = csv.writer(f3, delimiter="\t")

def _components(data, p=1, phi=0.0, axis=None, weights=None):
    # Utility function for computing the generalized rectangular components
    # of the circular data.
    if weights is None:
        weights = np.ones((1,))
    try:
        weights = np.broadcast_to(weights, data.shape)
    except ValueError:
        raise ValueError("Weights and data have inconsistent shape.")

    C = np.sum(weights * np.cos(p * (data - phi)), axis) / np.sum(weights, axis)
    S = np.sum(weights * np.sin(p * (data - phi)), axis) / np.sum(weights, axis)

    return C, S


def _angle(data, p=1, phi=0.0, axis=None, weights=None):
    # Utility function for computing the generalized sample mean angle
    C, S = _components(data, p, phi, axis, weights)

    # theta will be an angle in the interval [-np.pi, np.pi)
    # [-180, 180)*u.deg in case data is a Quantity
    theta = np.arctan2(S, C)

    if isinstance(data, Quantity):
        theta = theta.to(data.unit)

    return theta



azimuth_deg_list = []
elevation_deg_list = []

with open(args.coordinates, 'r') as crd:
    for line in crd:
        x=line.split('\t')
        azimuth_deg_list.append(float(x[0]))
        elevation_deg_list.append(float(x[1].strip()))

def min_angle_difference(angle1, angle2): # 0-180
    # Normalize angles to be between 0 and 360 degrees
    angle1 %= 360
    angle2 %= 360
    
    # Calculate the absolute difference between the normalized angles
    absolute_difference = abs(angle1 - angle2)
    
    # If the absolute difference is greater than 180 degrees,
    # subtract it from 360 degrees to get the minimum difference
    if absolute_difference > 180:
        return 360 - absolute_difference
    else:
        return absolute_difference

def min_angle_diff_0_90(a, b): # 0-90
    """
    Returns the minimum angular difference between two angles a and b,
    in degrees, always between 0 and 90.
    """
    # Normalize angles to 0-360
    a_norm = a % 360
    b_norm = b % 360
    
    # Compute absolute difference
    diff = abs(a_norm - b_norm)
    # Reduce modulo 180
    diff = diff % 180
    
    # Clamp to 0-90
    min_diff = min(diff, 180 - diff)
    
    return min_diff

def forstokes_eye_1(img_dmc):
    intensities_000 = []
    intensities_045 = []
    intensities_090 = []
    intensities_135 = []

    with subprocess.Popen(str('python realistic_FOV_aep_tissot_multiple_omm_intensities_forstokes_gaussian_sensitivity.py ' + img_dmc + ' ' + args.coordinates + ' 25 ' + str(rotation_angle) + ' ' + args.threads + ' ' + args.sigma), shell=True, stdout=subprocess.PIPE, text=True) as process:
        for line in process.stdout:
            if '000' in img_dmc:
                intensities_000.append(line.strip())
            elif '045' in img_dmc:
                intensities_045.append(line.strip())
            elif '090' in img_dmc:
                intensities_090.append(line.strip())
            elif '135' in img_dmc:
                intensities_135.append(line.strip())

    return intensities_000, intensities_045, intensities_090, intensities_135

def forstokes_eye_2(img_dmc):
    intensities_000_2 = []
    intensities_045_2 = []
    intensities_090_2 = []
    intensities_135_2 = []

    with subprocess.Popen(str('python realistic_FOV_aep_tissot_multiple_omm_intensities_forstokes_2ndeye_gaussian_sensitivity.py ' + img_dmc + ' ' + args.coordinates + ' 25 ' + str(rotation_angle) + ' ' + args.threads + ' ' + args.sigma), shell=True, stdout=subprocess.PIPE, text=True) as process:
        for line in process.stdout:
            if '000' in img_dmc:
                intensities_000_2.append(line.strip())
            elif '045' in img_dmc:
                intensities_045_2.append(line.strip())
            elif '090' in img_dmc:
                intensities_090_2.append(line.strip())
            elif '135' in img_dmc:
                intensities_135_2.append(line.strip())

    return intensities_000_2, intensities_045_2, intensities_090_2, intensities_135_2

circmeans = []
circmeans_2 = []
circmeans_botheyes = []
all_PRC_values = []
all_PRC_2_values = []
rotation_angles = []
all_saz_estimates = []
absolute_errors = []
total_vector_lengths = []
all_PRC_both_values = []

for rotation_angle in range(0, 360, 5):
    solar_azimuth = float(args.solarazimuth) + rotation_angle # frame of reference same as normal solar azimuth (increasing counterclockwise from up)
    if solar_azimuth < 360:
        rotation_angles.append(solar_azimuth)
    else:
        rotation_angles.append(solar_azimuth -360)
    S1 = []
    S2 = []
    S0 = []
    dolp = []
    aolp = []
    aolp_og =[]
    phimax_list = []
    phimax_2_list = []
    S_list = [] # polarization sensitivity
    S2_list = [] # polarization sensitivity perpendicular to S
    PRC = [] # photoreceptor contrast

    with Pool(processes=int(args.threads)) as pool:
        eye_results_1 = pool.map(forstokes_eye_1, args.demosaiced)
        eye_results_2 = pool.map(forstokes_eye_2, args.demosaiced)

    img_000_intensities = []
    img_045_intensities = []
    img_090_intensities = []
    img_135_intensities = []

    img_000_intensities_2 = []
    img_045_intensities_2 = []
    img_090_intensities_2 = []
    img_135_intensities_2 = []

    for intensities_000, intensities_045, intensities_090, intensities_135 in eye_results_1:
        img_000_intensities.extend(intensities_000)
        img_045_intensities.extend(intensities_045)
        img_090_intensities.extend(intensities_090)
        img_135_intensities.extend(intensities_135)

    for intensities_000_2, intensities_045_2, intensities_090_2, intensities_135_2 in eye_results_2:
        img_000_intensities_2.extend(intensities_000_2)
        img_045_intensities_2.extend(intensities_045_2)
        img_090_intensities_2.extend(intensities_090_2)
        img_135_intensities_2.extend(intensities_135_2)
        
    for x, y in zip(img_000_intensities, img_090_intensities):
        S1.append(float(x) - float(y))
    for z, w in zip(img_045_intensities, img_135_intensities):
        S2.append(float(z) - float(w))

    S0 = [(float(x1) + float(y1) + float(z1) + float(w1)) / 2 for x1, y1, z1, w1 in zip(img_000_intensities, img_045_intensities, img_090_intensities, img_135_intensities)]
    
    dolp = [ math.sqrt(x2**2 + y2**2) / z2 for x2, y2, z2 in zip(S1, S2, S0)]
    aolp = [ np.mod(((math.atan2(x3, y3) / 2) + np.radians(rotation_angle)), np.pi) / np.pi for x3, y3 in zip(S2, S1)] # divide by pi for colors, mod for values between 0 and 180
    aolp_og = [ np.mod(((math.atan2(x3, y3) / 2) + np.radians(rotation_angle)), np.pi) for x3, y3 in zip(S2, S1)]
    aolp_hist = [ np.mod(((math.atan2(x3, y3) / 2) + np.radians(rotation_angle)), np.pi) for x3, y3 in zip(S2, S1)] 
    aolp_lines = [ (math.atan2(x3, y3) / 2) + np.radians(rotation_angle) for x3, y3 in zip(S2, S1)]
    aolp_circmeans = [ np.mod(((math.atan2(x3, y3) / 2)), np.pi) for x3, y3 in zip(S2, S1)] # no rotation angle for circmeans
    
    # this is for the second eye
    S1_2 = []
    S2_2 = []
    S0_2 = []
    dolp_2 = []
    aolp_2 = []
    aolp_2_og = []
    S_2_list = [] # polarization sensitivity
    S2_2_list = [] # polarization sensitivity perpendicular to S
    PRC_2 = [] # photoreceptor contrast second eye
      
    for x, y in zip(img_000_intensities_2, img_090_intensities_2):
        S1_2.append(float(x) - float(y))
    for z, w in zip(img_045_intensities_2, img_135_intensities_2):
        S2_2.append(float(z) - float(w))

    S0_2 = [(float(x1) + float(y1) + float(z1) + float(w1)) / 2 for x1, y1, z1, w1 in zip(img_000_intensities_2, img_045_intensities_2, img_090_intensities_2, img_135_intensities_2)]
    dolp_2 = [ math.sqrt(x2**2 + y2**2) / z2 for x2, y2, z2 in zip(S1_2, S2_2, S0_2)]
    aolp_2 = [ np.mod(((math.atan2(x3, y3) / 2) + np.radians(rotation_angle)), np.pi) / np.pi for x3, y3 in zip(S2_2, S1_2)] # divide by pi for colors , mod for values between 0 and 180
    aolp_2_og = [ np.mod(((math.atan2(x3, y3) / 2) + np.radians(rotation_angle)), np.pi) for x3, y3 in zip(S2_2, S1_2)]
    aolp_2_hist = [ np.mod(((math.atan2(x3, y3) / 2) + np.radians(rotation_angle)), np.pi) for x3, y3 in zip(S2_2, S1_2)] 
    aolp_2_lines = [ (math.atan2(x3, y3) / 2) + np.radians(rotation_angle) for x3, y3 in zip(S2_2, S1_2)]
    aolp_2_circmeans = [ np.mod(((math.atan2(x3, y3) / 2)), np.pi) for x3, y3 in zip(S2_2, S1_2)] # no rotation angle for circmeans

            
    aolp_both_eyes = aolp_circmeans + aolp_2_circmeans
    dolp_both_eyes = dolp + dolp_2

    
    os.system('python realistic_FOV_aep_tissot_multiple_colors_aolp_phimax_newmodel.py ' + args.input + ' aolp_1steye_' + str(rotation_angle) + '.png "' + str(azimuth_deg_list) + '" "' + str(elevation_deg_list) + '" "' + str(aolp) + '" 25')

    if len(phimax_list) == 0: # don't run the script if it already exists
        with open("azimuth_elevation_slope_data.csv", 'r') as phimax_values:
            for line in phimax_values:
                x = line.split('\t')
                phimax_list.append(float(x[3]))
                phimax_2_list.append(float(x[4]))
            
    # for the first eye
    angles4_1 = []
    for aolp_value, dolp_value, phimax_value, phimax_2_value in zip(aolp_lines, dolp, phimax_list, phimax_2_list):

        # this to cacluate fanshape_match
        diff = np.radians( ((270 - np.degrees(aolp_value)) % 180 - (phimax_value % 180)) )
        a_diff = np.arctan2(np.sin(diff), np.abs(np.cos(diff)))
        a_diff_deg = np.rad2deg(a_diff)
        abs_a_diff = np.abs(a_diff_deg)
        if abs_a_diff > 45: # aolp is closer to phimax_2, make phimax_2=aolp, and phimax=aolp+90
            S_list.append(float(1 + ((dolp_value * (Sp - 1)) / (Sp + 1)) * 
                np.cos(2 * (np.radians(270) - aolp_value) - 2 * (np.radians(270) - aolp_value + np.radians(90)))))
            S2_list.append(float(1 + ((dolp_value * (Sp - 1)) / (Sp + 1)) * 
                np.cos(2 * (np.radians(270) - aolp_value) - 2 * (np.radians(270) - aolp_value))))
        else: # aolp is closer to phimax, make phimax=aolp and phimax_2=aolp+90
            S_list.append(float(1 + ((dolp_value * (Sp - 1)) / (Sp + 1)) * 
                np.cos(2 * (np.radians(270) - aolp_value) - 2 * (np.radians(270) - aolp_value))))
            S2_list.append(float(1 + ((dolp_value * (Sp - 1)) / (Sp + 1)) * 
                np.cos(2 * (np.radians(270) - aolp_value) - 2 * (np.radians(270) - aolp_value + np.radians(90)))))
        angle4 = abs_a_diff * 4
        angles4_1.append(np.radians(angle4))



    mean_match_1 = circmean(np.array(angles4_1)) / 4
    
        
    for S_value, S_2_value in zip(S_list, S2_list):
        PRC.append(float(np.log(S_value / S_2_value)))
    PRC_scaled = np.interp(PRC, (-np.log(Sp*0.7),np.log(Sp*0.7)), (0,1)) # we use the log here, derived from theoretical max ratio, multiplying by 0.7 which is typical max DoLP in the sky
    PRC_scaled = PRC_scaled.tolist()
    #print(PRC_scaled)
    all_PRC_values.append(PRC)

    # for the second eye
    angles4_2 = []
    for aolp_value, dolp_value, phimax_value, phimax_2_value in zip(aolp_2_lines, dolp_2, phimax_list, phimax_2_list):
        # this to cacluate fanshape_match
        diff = np.radians( ((270 - np.degrees(aolp_value)) % 180 - (-phimax_value % 180)) )
        a_diff = np.arctan2(np.sin(diff), np.abs(np.cos(diff)))
        a_diff_deg = np.rad2deg(a_diff)
        abs_a_diff = np.abs(a_diff_deg)
        if abs_a_diff > 45: # aolp is closer to phimax_2, make phimax_2=aolp, and phimax=aolp+90
            S_2_list.append(float(1 + ((dolp_value * (Sp - 1)) / (Sp + 1)) * 
                np.cos(2 * (np.radians(270) - aolp_value) - 2 * (np.radians(270) - aolp_value + np.radians(90))))) #=-1
            S2_2_list.append(float(1 + ((dolp_value * (Sp - 1)) / (Sp + 1)) * 
                np.cos(2 * (np.radians(270) - aolp_value) - 2 * (np.radians(270) - aolp_value)))) # =1
        else: # aolp is closer to phimax, make phimax=aolp and phimax_2=aolp+90
            S_2_list.append(float(1 + ((dolp_value * (Sp - 1)) / (Sp + 1)) * 
                np.cos(2 * (np.radians(270) - aolp_value) - 2 * (np.radians(270) - aolp_value)))) #=1
            S2_2_list.append(float(1 + ((dolp_value * (Sp - 1)) / (Sp + 1)) * 
                np.cos(2 * (np.radians(270) - aolp_value) - 2 * (np.radians(270) - aolp_value + np.radians(90))))) #=-1
        angle4 = abs_a_diff * 4
        angles4_2.append(np.radians(angle4))

    mean_match_2 = circmean(np.array(angles4_2)) / 4
    
        
    for S_value, S_2_value in zip(S_2_list, S2_2_list):
        PRC_2.append(float(np.log(S_value / S_2_value)))
    PRC_2_scaled = np.interp(PRC_2, (-np.log(Sp*0.7),np.log(Sp*0.7)), (0,1)) # we use the log here, derived from theoretical max ratio, multiplying by 0.7 which is typical max DoLP in the sky
    PRC_2_scaled = PRC_2_scaled.tolist()
    all_PRC_2_values.append(PRC_2)
    
    command = str('python realistic_FOV_aep_tissot_multiple_colors_aolp_phimax_contrast_saz_newmodel.py ' + args.input + ' aolp_1steye_' + str(rotation_angle) + '.png "' + str(azimuth_deg_list) + '" "' + str(elevation_deg_list) + '" "' + str(PRC) + '" 25 ' + str(rotation_angle) + ' "' + str(PRC_scaled) + '"')
    with subprocess.Popen(command, shell=True, stdout=subprocess.PIPE, text=True) as process:
        output = process.stdout.read().strip()
    before_list, _, list_part = output.partition('[')
    first_eye_saz_x, first_eye_saz_y, first_eye_angle = before_list.strip().split()
    first_eye_ommatidia_angles_list = ast.literal_eval('[' + list_part)
    
    #os.system('python realistic_FOV_aep_tissot_multiple_colors_aolp_lines.py ' + args.input + ' aolp_1steye_' + str(rotation_angle) + '_lines.png "' + str(azimuth_deg_list) + '" "' + str(elevation_deg_list) + '" "' + str(aolp_lines) + '" "' + str(dolp) + '"')
    #os.system('python make_mask_transparent.py aolp_1steye_' + str(rotation_angle) + '.png aolp_1steye_' + str(rotation_angle) + '_transparent.png')
    #os.system('python make_mask_transparent.py aolp_1steye_' + str(rotation_angle) + '_lines.png aolp_1steye_' + str(rotation_angle) + '_lines_transparent.png')
    #os.system('rm aolp_1steye_' + str(rotation_angle) + '.png')
    #os.system('rm aolp_1steye_' + str(rotation_angle) + '_lines.png')
    
    # Open the circular image
    img = cv2.imread(args.input_viz)
    
    # Calculate the center of the projection
    img_height, img_width, _ = img.shape
    center_x = img_width // 2
    center_y = img_height // 2

    # this is for rotating the image if necessary (bicubic interpolation). Note that it rotates counterclockwise for positive angles
    M = cv2.getRotationMatrix2D((center_y,center_x),rotation_angle,1) # the format is cv2.getRotationMatrix2D(center, angle, scale) 
    img = cv2.warpAffine(img,M,(img_width,img_height),flags=cv2.INTER_CUBIC)
    #cv2.imwrite('1steye_' + str(rotation_angle) + '_background.png', img)

    #os.system('python image_superimposing.py 1steye_' + str(rotation_angle) + '_background.png aolp_1steye_' + str(rotation_angle) + '_transparent.png aolp_1steye_' + str(rotation_angle) + '_background.png')
    #os.system('python image_superimposing.py aolp_1steye_' + str(rotation_angle) + '_background.png aolp_1steye_' + str(rotation_angle) + '_lines_transparent.png aolp_1steye_' + str(rotation_angle) + '_background.png')
    #os.system('rm 1steye_' + str(rotation_angle) + '_background.png')
    #os.system('rm aolp_1steye_' + str(rotation_angle) + '_transparent.png')
    #os.system('rm aolp_1steye_' + str(rotation_angle) + '_lines_transparent.png')

    command = str('python realistic_FOV_aep_tissot_multiple_colors_2ndeye_aolp_phimax_contrast_saz_newmodel.py ' + args.input + ' aolp_2ndeye_' + str(rotation_angle) + '.png "' + str(azimuth_deg_list) + '" "' + str(elevation_deg_list) + '" "' + str(PRC_2) + '" 25 ' + str(rotation_angle) + ' ' + str(first_eye_saz_x) + ' ' + str(first_eye_saz_y) + ' "' + str(PRC_2_scaled) + '"')
    with subprocess.Popen(command, shell=True, stdout=subprocess.PIPE, text=True) as process:
        output = process.stdout.read().strip()
    before_list, _, list_part = output.partition('[')
    total_vector_angle, total_vector_length, second_eye_angle = before_list.strip().split()
    second_eye_ommatidia_angles_list = ast.literal_eval('[' + list_part)

    all_saz_estimates.append(float(total_vector_angle)+float(np.radians(rotation_angle)))
    total_vector_lengths.append(float(total_vector_length))
    absolute_error=min_angle_difference(-float(float(args.solarazimuth)+float(rotation_angle)), np.degrees(float(total_vector_angle)))
    absolute_errors.append(absolute_error) # this modification is due to the difference in points of reference in the two systems (vectors increase clockwise from right and saz increases counterclockwise from up)

    #os.system('python realistic_FOV_aep_tissot_multiple_colors_2ndeye_aolp_lines.py ' +args.input + ' aolp_2ndeye_' + str(rotation_angle) + '_lines.png "' + str(azimuth_deg_list) + '" "' + str(elevation_deg_list) + '" "' + str(aolp_2_lines) + '" "' + str(dolp_2) + '"')
    #os.system('python make_mask_transparent.py aolp_2ndeye_' + str(rotation_angle) + '.png aolp_2ndeye_' + str(rotation_angle) + '_transparent.png')
    #os.system('python make_mask_transparent.py aolp_2ndeye_' + str(rotation_angle) + '_lines.png aolp_2ndeye_' + str(rotation_angle) + '_lines_transparent.png')
    #os.system('rm aolp_2ndeye_' + str(rotation_angle) + '.png')
    #os.system('rm aolp_2ndeye_' + str(rotation_angle) + '_lines.png')

    #os.system('python image_superimposing.py aolp_2ndeye_' + str(rotation_angle) + '_transparent.png aolp_2ndeye_' + str(rotation_angle) + '_lines_transparent.png aolp_2ndeye_' + str(rotation_angle) + '_transparent.png')
    
    #os.system('python image_superimposing.py aolp_1steye_' + str(rotation_angle) + '_background.png aolp_2ndeye_' + str(rotation_angle) + '_transparent.png aolp_' + str(rotation_angle) + '_background.png')
    #os.system('rm aolp_2ndeye_' + str(rotation_angle) + '_transparent.png')
    #os.system('rm aolp_2ndeye_' + str(rotation_angle) + '_lines_transparent.png')
    #os.system('rm aolp_1steye_' + str(rotation_angle) + '_background.png')
    #os.system('python circular_masking.py aolp_' + str(rotation_angle) + '_background.png aolp_' + str(rotation_angle) + '_background_transparent.png')
    #os.system('rm aolp_' + str(rotation_angle) + '_background.png')
    # frame to import is aolp_' + str(rotation_angle) + '_background_transparent.png



    first_eye_abs_error = min_angle_difference(-float(float(args.solarazimuth)+float(rotation_angle)), np.degrees(float(first_eye_angle)))
    second_eye_abs_error = min_angle_difference(-float(float(args.solarazimuth)+float(rotation_angle)), np.degrees(float(second_eye_angle)))

    first_eye_all_ommatidia_abs_error = [min_angle_difference(-float(float(args.solarazimuth)+float(rotation_angle)), np.degrees(float(angle))) for angle in first_eye_ommatidia_angles_list]

    second_eye_all_ommatidia_abs_error = [min_angle_difference(-float(float(args.solarazimuth)+float(rotation_angle)), np.degrees(float(angle))) for angle in second_eye_ommatidia_angles_list]
    
    #print(sum(S0), first_eye_abs_error)
    #print(sum(S0_2), second_eye_abs_error)
    
    
    with open('first_eye_ommatidia_intensity_errors.tsv', 'a', newline='') as tsvfile:
        writer = csv.writer(tsvfile, delimiter='\t')
        for row in zip(S0, first_eye_all_ommatidia_abs_error):
            writer.writerow(list(row) + [first_eye_abs_error])
    with open('second_eye_ommatidia_intensity_errors.tsv', 'a', newline='') as tsvfile:
        writer = csv.writer(tsvfile, delimiter='\t')
        for row in zip(S0_2, second_eye_all_ommatidia_abs_error):
            writer.writerow(list(row) + [second_eye_abs_error])

    mean_match  = circmean(np.array([mean_match_1*4, mean_match_2*4]))/4
    writer1.writerow([mean_match_1, first_eye_abs_error, float(args.solarazimuth)+float(rotation_angle)])
    writer2.writerow([mean_match_2, second_eye_abs_error, float(args.solarazimuth)+float(rotation_angle)])

    writer3.writerow([mean_match, absolute_error, float(args.solarazimuth)+float(rotation_angle)])

    with open("all_aolp_botheyes.txt", "a") as f:
        # stack both lists
        all_angles = np.array(aolp_og + aolp_2_og)

        # double angles (map from [0,π) to [0,2π))
        doubled = 2 * all_angles
    
        # compute circular mean of doubled
        mean_angle = math.atan2(np.sin(doubled).mean(), np.cos(doubled).mean())
    
        # recenter around mean
        centered = doubled - mean_angle
    
        # wrap into (-π, π)
        wrapped = (centered + math.pi) % (2*math.pi) - math.pi
    
        # halve back to original AoP space
        unfolded = wrapped / 2
    
        # now compute range correctly
        global_min = unfolded.min()
        global_max = unfolded.max()
        value_range = global_max - global_min

        # compute the atan2 expression
        atan_val = np.arctan2(
            np.sin(np.deg2rad(absolute_error)),
            abs(np.cos(np.deg2rad(absolute_error)))
        )

        # write values separated by tabs
        f.write(
            str(value_range) + "\t" +
            str(absolute_error) + "\t" +
            str(atan_val) + "\n" # bimodal error
        )

    # this for Fisher information
    

    PRC_both = np.concatenate((np.array(PRC), np.array(PRC_2)))
    all_PRC_both_values.append(PRC_both)
    
f1.close()
f2.close()
f3.close()

## for Fisher info
delta_phi = np.radians(5)  # since your rotation step is 5 degrees

FI_total = []  # Fisher Information for combined eyes

# Loop over successive rotations to compute finite differences
for k in range(len(all_PRC_both_values) - 1):
    PRC_now = np.array(all_PRC_both_values[k])
    PRC_next = np.array(all_PRC_both_values[k + 1])
    
    dR_dphi = (PRC_next - PRC_now) / delta_phi  # finite difference derivative
    FI_total.append(np.sum(dR_dphi ** 2))       # sum of squared sensitivity over all ommatidia

# Convert to NumPy array for convenience
FI_total = np.array(FI_total)

# Compute predicted minimal orientation error (Cramér-Rao bound)
predicted_error = 1 / np.sqrt(FI_total)

# Ensure arrays are NumPy arrays
rotation_angles_mid = np.array(rotation_angles[:-1])
predicted_error = np.array(predicted_error)

# Save to TSV
with open('predicted_error_per_azimuth.tsv', 'w', newline='') as tsvfile:
    writer = csv.writer(tsvfile, delimiter='\t')
    # Write header
    writer.writerow(['Sun_Azimuth_deg', 'Predicted_Error'])
    # Write data
    for az, err in zip(rotation_angles_mid, predicted_error):
        writer.writerow([az, err])




# Flatten the lists of PRC and PRC_2 values (make list out of list of lists)
flat_PRC_values = [item for sublist in all_PRC_values for item in sublist]
flat_PRC_2_values = [item for sublist in all_PRC_2_values for item in sublist]

# Plotting the scatter plot
plt.scatter(np.repeat(rotation_angles, len(PRC)), flat_PRC_values, c='blue', label='right_eye', s=5)
plt.scatter(np.repeat(rotation_angles, len(PRC_2)), flat_PRC_2_values, c='red', label='left_eye', s=5)

# Adding labels and legend
plt.xlabel('solar azimuth (degrees)')
plt.ylabel('PRC Values')
plt.legend()

plt.savefig('PRC_scatter_plot.png')

# we need to sort the solar azimuths (rotation_angles) but simultaneously sort the PRC values.
rotation_angles_PRC_values = list(zip(rotation_angles, all_PRC_values))
rotation_angles_PRC_2_values = list(zip(rotation_angles, all_PRC_2_values))
rotation_angles_PRC_values.sort(key=lambda x: x[0])
rotation_angles_PRC_2_values.sort(key=lambda x: x[0])
sorted_rotation_angles = [item[0] for item in rotation_angles_PRC_values]
sorted_all_PRC_values = [item[1] for item in rotation_angles_PRC_values]
sorted_all_PRC_2_values = [item[1] for item in rotation_angles_PRC_2_values]

# Create a single graph for both eyes
plt.figure()
for i in range(len(all_PRC_values[0])):
    plt.plot(sorted_rotation_angles, [lst[i] for lst in sorted_all_PRC_values], alpha=0.7, c = 'blue')

for i in range(len(all_PRC_2_values[0])):
    plt.plot(sorted_rotation_angles, [lst[i] for lst in sorted_all_PRC_2_values], alpha=0.7, c = 'red')

plt.title('Both Eyes - All Ommatidia')
plt.xlabel('solar azimuth (degrees)')
plt.ylabel('PRC value')
# Set x-axis ticks at every ten degrees
plt.xticks(range(0, 361, 30))
plt.savefig('both_eyes_all_ommatidia.png')
plt.close()

# Create a single graph for all PRC differences (between pairs of ommatidia)
plt.figure()
for i in range(len(all_PRC_2_values[0])):
    # Plot the difference between right and left eye values
    difference_PRC_values = [lst1[i] - lst2[i] for lst1, lst2 in zip(sorted_all_PRC_values, sorted_all_PRC_2_values)]
    plt.plot(sorted_rotation_angles, difference_PRC_values, c='green')

plt.title('PRC difference between pairs of ommatidia')
plt.xlabel('solar azimuth (degrees)')
plt.ylabel('PRC difference value')
# Set x-axis ticks at every ten degrees
plt.xticks(range(0, 361, 30))
plt.savefig('PRC_difference.png')
plt.close()

all_saz_estimates = np.array(all_saz_estimates) 
all_saz_estimates = np.pi/2 - all_saz_estimates # modify the estimates to match the scatter plot which has 0deg right increasing counterclockwise
circstd_value = np.degrees(float(circstd(all_saz_estimates))) # circular standard deviation

# plot the saz estimates along with the circstd
fig, ax = plt.subplots()

ax.plot(np.cos(np.linspace(0, 2*np.pi, 500)),
        np.sin(np.linspace(0, 2*np.pi, 500)),
        c='k')

ax.scatter(np.cos(all_saz_estimates), np.sin(all_saz_estimates), c='k', s=15, alpha=0.5)
ax.plot([0, np.cos(np.radians(float(args.solarazimuth)-270))], [0, np.sin(np.radians(float(args.solarazimuth)-270))], c='green')
ax.set_title(f"circular std: {np.round(circstd_value, 2)!r}°, mean error: {np.round(statistics.mean(absolute_errors), 2)!r}°", y=1.05)

# Add labels
ax.text(0, 1.1, '0°', ha='center')
ax.text(-1.1, 0, '90°', va='center', ha='right')
ax.text(0, -1.1, '180°', ha='center', va='top')
ax.text(1.1, 0, '270°', va='center')
ax.axis('equal')
ax.axis('off')

plt.savefig('circstd.png')
plt.close()
    
# plot absolute errors (saz-estimate) as a function of total vector lengths of estimates
plt.figure(figsize=(8, 6))
plt.plot(total_vector_lengths,  absolute_errors, 'o', markersize=8)

# Perform linear regression
slope, intercept, r_value, p_value, std_err = stats.linregress(total_vector_lengths, absolute_errors)
line = slope * np.array(total_vector_lengths) + intercept
plt.plot(total_vector_lengths, line, 'r', label='Regression line')

plt.xlabel('Vector Length (Azimuth estimate)')
plt.ylabel('Absolute Error (degrees)')
plt.grid(True)
plt.legend()
plt.savefig('saz_vector_lengths_errors_with_regression.png')
plt.close()

# plot absolute errors (saz-estimate) as a function of solar azimuth estimates
plt.figure(figsize=(8, 6))
plt.plot(rotation_angles,  absolute_errors, 'o', markersize=8)

plt.xlabel('Solar azimuth (degrees)')
plt.ylabel('Absolute Error (degrees)')
plt.grid(True)
plt.savefig('saz_error.png')
plt.close()
