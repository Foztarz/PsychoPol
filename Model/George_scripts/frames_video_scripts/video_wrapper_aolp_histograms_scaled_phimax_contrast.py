## usage: python script.py -i HDR.png -dmc img_000_cropped.png img_045_cropped.png img_090_cropped.png img_135_cropped.png -c coordinates.txt

# this script makes frames for each rotation angle (of the original image) where FOVs/ellipses are colored based on PRC (photoreceptor contrast) and in each FOV (ellipse) we have lines that correspond to AoLP (egocentric).
# The thickness of the lines is based on DoLP

import os
import sys
import argparse
import cv2
import numpy as np
import subprocess
import math
import matplotlib.pyplot as plt
import scipy
from astropy.units import Quantity

parser = argparse.ArgumentParser()
parser.add_argument("-i", "--input", required=True, help="INPUT must be the input sky image. It must be square with transparent edges. (required)")
parser.add_argument("-dmc", "--demosaiced", nargs='+', required=True, help="DEMOSAICED must be the 4 input CROPPED demosaiced images (000,045,090,135). (required)")
parser.add_argument("-c", "--coordinates", required=True, help="COORDINATES must be a text file with two columns, tab-separated. Each line contains coordinates (azimuth, elevation) for the FOV of one ommatidium. (required)")
args=parser.parse_args()

Sp = 6.6 # polarization sensitivity as defined in Labhart, 1980

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

def circmean(data, axis=None, weights=None):
    return _angle(data, 1, 0.0, axis, weights)

azimuth_deg_list = []
elevation_deg_list = []

with open(args.coordinates, 'r') as crd:
    for line in crd:
        x=line.split('\t')
        azimuth_deg_list.append(float(x[0]))
        elevation_deg_list.append(float(x[1].strip()))
#print(azimuth_deg_list)
#print(elevation_deg_list)
circmeans = []
circmeans_2 = []
circmeans_botheyes = []
for rotation_angle in range(0, 360, 5):
    img_000_intensities = []
    img_045_intensities = []
    img_090_intensities = []
    img_135_intensities = []
    S1 = []
    S2 = []
    S0 = []
    dolp = []
    aolp = []
    phimax_list = []
    phimax_2_list = []
    S_list = [] # polarization sensitivity
    S2_list = [] # polarization sensitivity perpendicular to S
    PRC = [] # photoreceptor contrast
    for img_dmc in args.demosaiced:
        # Open a subprocess to run the command and capture output line by line
        with subprocess.Popen(str('python realistic_FOV_aep_tissot_multiple_omm_intensities_forstokes.py ' + img_dmc + ' ' + args.coordinates + ' 25 ' + str(rotation_angle)), shell=True, stdout=subprocess.PIPE, text=True) as process:
            if '000' in img_dmc:
                for line in process.stdout:
                    # Append each intensity to the list
                    img_000_intensities.append(line.strip())
            elif '045' in img_dmc:
                for line in process.stdout:
                    # Append each intensity to the list
                    img_045_intensities.append(line.strip())
            elif '090' in img_dmc:
                for line in process.stdout:
                    # Append each intensity to the list
                    img_090_intensities.append(line.strip())
            elif '135' in img_dmc:
                for line in process.stdout:
                    # Append each intensity to the list
                    img_135_intensities.append(line.strip())
        
    for x, y in zip(img_000_intensities, img_090_intensities):
        S1.append(float(x) - float(y))
    for z, w in zip(img_045_intensities, img_135_intensities):
        S2.append(float(z) - float(w))

    S0 = [(float(x1) + float(y1) + float(z1) + float(w1)) / 2 for x1, y1, z1, w1 in zip(img_000_intensities, img_045_intensities, img_090_intensities, img_135_intensities)]
    dolp = [ math.sqrt(x2**2 + y2**2) / z2 for x2, y2, z2 in zip(S1, S2, S0)]
    aolp = [ np.mod(((math.atan2(x3, y3) / 2) - np.radians(rotation_angle)), np.pi) / np.pi for x3, y3 in zip(S2, S1)] 
    aolp_lines = [ (math.atan2(x3, y3) / 2) - np.radians(rotation_angle) for x3, y3 in zip(S2, S1)]
    
    # Calculate the polar histogram for aolp list
    N = 59
    aop_hist = np.histogram(aolp, bins=N, range=[-np.pi, np.pi])

    bottom = 0
    max_height = 1102

    theta = np.linspace(-np.pi, np.pi, N, endpoint=False)
    radii = aop_hist[0]
    width = 0.1 * (2 * np.pi) / N

    # Plot the polar histogram
    fig = plt.figure()
    ax = fig.add_subplot(111, polar=True)
    bars = ax.bar(theta, radii, width=width, bottom=bottom)
    ax.set_rlabel_position(270)

    # Use custom colors and opacity
    for r, bar in zip(radii, bars):
        bar.set_facecolor('red')
        
    # Set title with rotation angle
    ax.set_title('aolp_{}_right_eye'.format(rotation_angle))
    
    ax.set_xlabel("Angle of Polarization")

    # Save the polar histogram
    output_path = 'polar_histogram_{}_1steye.png'.format(rotation_angle)
    fig.savefig(output_path)
    plt.close(fig)
        
    # this is for the second eye
    img_000_intensities_2 = []
    img_045_intensities_2 = []
    img_090_intensities_2 = []
    img_135_intensities_2 = []
    S1_2 = []
    S2_2 = []
    S0_2 = []
    dolp_2 = []
    aolp_2 = []
    S_2_list = [] # polarization sensitivity
    S2_2_list = [] # polarization sensitivity perpendicular to S
    PRC_2 = [] # photoreceptor contrast second eye
    for img_dmc in args.demosaiced:
        # Open a subprocess to run the command and capture output line by line
        with subprocess.Popen(str('python realistic_FOV_aep_tissot_multiple_omm_intensities_forstokes_2ndeye.py ' + img_dmc + ' ' + args.coordinates + ' 25 ' + str(rotation_angle)), shell=True, stdout=subprocess.PIPE, text=True) as process:
            if '000' in img_dmc:
                for line in process.stdout:
                    # Append each intensity to the list
                    img_000_intensities_2.append(line.strip())
            elif '045' in img_dmc:
                for line in process.stdout:
                    # Append each intensity to the list
                    img_045_intensities_2.append(line.strip())
            elif '090' in img_dmc:
                for line in process.stdout:
                    # Append each intensity to the list
                    img_090_intensities_2.append(line.strip())
            elif '135' in img_dmc:
                for line in process.stdout:
                    # Append each intensity to the list
                    img_135_intensities_2.append(line.strip())
      
    for x, y in zip(img_000_intensities_2, img_090_intensities_2):
        S1_2.append(float(x) - float(y))
    for z, w in zip(img_045_intensities_2, img_135_intensities_2):
        S2_2.append(float(z) - float(w))

    S0_2 = [(float(x1) + float(y1) + float(z1) + float(w1)) / 2 for x1, y1, z1, w1 in zip(img_000_intensities_2, img_045_intensities_2, img_090_intensities_2, img_135_intensities_2)]
    dolp_2 = [ math.sqrt(x2**2 + y2**2) / z2 for x2, y2, z2 in zip(S1_2, S2_2, S0_2)]
    aolp_2 = [ np.mod(((math.atan2(x3, y3) / 2) - np.radians(rotation_angle)), np.pi) / np.pi for x3, y3 in zip(S2_2, S1_2)]
    aolp_2_lines = [ (math.atan2(x3, y3) / 2) - np.radians(rotation_angle) for x3, y3 in zip(S2_2, S1_2)]
    
    # Calculate the polar histogram for aolp_2 list
    N = 59
    aop_hist = np.histogram(aolp_2, bins=N, range=[-np.pi, np.pi])

    bottom = 0
    max_height = 1102

    theta = np.linspace(-np.pi, np.pi, N, endpoint=False)
    radii = aop_hist[0]
    width = 0.1 * (2 * np.pi) / N

    # Plot the polar histogram
    fig = plt.figure()
    ax = fig.add_subplot(111, polar=True)
    bars = ax.bar(theta, radii, width=width, bottom=bottom)
    ax.set_rlabel_position(270)

    # Use custom colors and opacity
    for r, bar in zip(radii, bars):
        bar.set_facecolor('red')
        
    # Set title with rotation angle
    ax.set_title('aolp_{}_left_eye'.format(rotation_angle))
    
    ax.set_xlabel("Angle of Polarization")

    # Save the polar histogram
    output_path = 'polar_histogram_{}_2ndeye.png'.format(rotation_angle)
    fig.savefig(output_path)
    plt.close(fig)

    aolp_both_eyes = aolp + aolp_2 
    dolp_both_eyes = dolp + dolp_2
    
    # Calculate the polar histogram for both eyes
    N = 59
    aop_hist_1 = np.histogram(aolp, bins=N, range=[-np.pi, np.pi])

    # Calculate the polar histogram for the second set of AOLP values
    aop_hist_2 = np.histogram(aolp_2, bins=N, range=[-np.pi, np.pi])

    bottom = 0
    max_height = max(max(aop_hist_1[0]), max(aop_hist_2[0]))

    theta = np.linspace(-np.pi, np.pi, N, endpoint=False)
    width = 0.1 * (2 * np.pi) / N

    # Plot the polar histogram for the first set of AOLP values (in red)
    fig = plt.figure()
    ax = fig.add_subplot(111, polar=True)
    bars1 = ax.bar(theta, aop_hist_1[0], width=width, bottom=bottom)
    for bar in bars1:
        bar.set_facecolor('red') # red for one eye

    # Plot the polar histogram for the second set of AOLP values (in green)
    bars2 = ax.bar(theta, aop_hist_2[0], width=width, bottom=bottom)
    for bar in bars2:
        bar.set_facecolor('blue') # blue for the other eye

    ax.set_rlabel_position(270)
    ax.set_title('aolp_{}_both_eyes'.format(rotation_angle))
    ax.set_xlabel("Angle of Polarization")

    # Save the polar histogram
    output_path = 'polar_histogram_{}_botheyes.png'.format(rotation_angle)
    fig.savefig(output_path)
    plt.close(fig)

    
    os.system('python realistic_FOV_aep_tissot_multiple_colors_aolp_phimax.py ' + args.input + ' aolp_1steye_' + str(rotation_angle) + '.png "' + str(azimuth_deg_list) + '" "' + str(elevation_deg_list) + '" "' + str(aolp) + '" 25')

    with open("azimuth_elevation_slope_data.csv", 'r') as phimax_values:
        for line in phimax_values:
            x = line.split('\t')
            phimax_list.append(float(x[3]))
            phimax_2_list.append(float(x[4]))
            
    # for the first eye
    for aolp_value, dolp_value, phimax_value, phimax_2_value in zip(aolp, dolp, phimax_list, phimax_2_list):
        S_list.append(float(1 + ((dolp_value*(Sp - 1)) / (Sp + 1)) * np.cos(2*aolp_value - 2*np.radians(phimax_value))))
        S2_list.append(float(1 + ((dolp_value*(Sp - 1)) / (Sp + 1)) * np.cos(2*aolp_value - 2*np.radians(phimax_2_value))))
    for S_value, S_2_value in zip(S_list, S2_list):
        PRC.append(float(np.log(S_value / S_2_value)))
    PRC_scaled = np.interp(PRC, (min(PRC), max(PRC)), (0,1))
    PRC_scaled = PRC_scaled.tolist()
    #print(PRC_scaled)
    # for the second eye
    for aolp_value, dolp_value, phimax_value, phimax_2_value in zip(aolp_2, dolp_2, phimax_list, phimax_2_list):
        S_2_list.append(float(1 + ((dolp_value*(Sp - 1)) / (Sp + 1)) * np.cos(2*aolp_value - 2*np.radians(phimax_value))))
        S2_2_list.append(float(1 + ((dolp_value*(Sp - 1)) / (Sp + 1)) * np.cos(2*aolp_value - 2*np.radians(phimax_2_value))))
    for S_value, S_2_value in zip(S_2_list, S2_2_list):
        PRC_2.append(float(np.log(S_value / S_2_value)))
    PRC_2_scaled = np.interp(PRC_2, (min(PRC_2), max(PRC_2)), (0,1))
    PRC_2_scaled = PRC_2_scaled.tolist()
    
    os.system('python realistic_FOV_aep_tissot_multiple_colors_aolp_phimax_contrast.py ' + args.input + ' aolp_1steye_' + str(rotation_angle) + '.png "' + str(azimuth_deg_list) + '" "' + str(elevation_deg_list) + '" "' + str(PRC_scaled) + '" 25')   
    os.system('python realistic_FOV_aep_tissot_multiple_colors_aolp_lines.py ' + args.input + ' aolp_1steye_' + str(rotation_angle) + '_lines.png "' + str(azimuth_deg_list) + '" "' + str(elevation_deg_list) + '" "' + str(aolp_lines) + '" "' + str(dolp) + '"')
    os.system('python make_mask_transparent.py aolp_1steye_' + str(rotation_angle) + '.png aolp_1steye_' + str(rotation_angle) + '_transparent.png')
    os.system('python make_mask_transparent.py aolp_1steye_' + str(rotation_angle) + '_lines.png aolp_1steye_' + str(rotation_angle) + '_lines_transparent.png')
    os.system('rm aolp_1steye_' + str(rotation_angle) + '.png')
    os.system('rm aolp_1steye_' + str(rotation_angle) + '_lines.png')
    
    # Open the circular image
    img = cv2.imread(args.input)        
    # Calculate the center of the projection
    img_height, img_width, _ = img.shape
    center_x = img_width // 2
    center_y = img_height // 2

    # this is for rotating the image if necessary (bicubic interpolation). Note that it rotates counterclockwise for positive angles
    M = cv2.getRotationMatrix2D((center_y,center_x),rotation_angle,1) # the format is cv2.getRotationMatrix2D(center, angle, scale) 
    img = cv2.warpAffine(img,M,(img_width,img_height),flags=cv2.INTER_CUBIC)
    cv2.imwrite('1steye_' + str(rotation_angle) + '_background.png', img)

    os.system('python image_superimposing.py 1steye_' + str(rotation_angle) + '_background.png aolp_1steye_' + str(rotation_angle) + '_transparent.png aolp_1steye_' + str(rotation_angle) + '_background.png')
    os.system('python image_superimposing.py aolp_1steye_' + str(rotation_angle) + '_background.png aolp_1steye_' + str(rotation_angle) + '_lines_transparent.png aolp_1steye_' + str(rotation_angle) + '_background.png')
    os.system('rm 1steye_' + str(rotation_angle) + '_background.png')
    os.system('rm aolp_1steye_' + str(rotation_angle) + '_transparent.png')
    os.system('rm aolp_1steye_' + str(rotation_angle) + '_lines_transparent.png')

    os.system('python realistic_FOV_aep_tissot_multiple_colors_2ndeye_aolp_phimax_contrast.py ' + args.input + ' aolp_2ndeye_' + str(rotation_angle) + '.png "' + str(azimuth_deg_list) + '" "' + str(elevation_deg_list) + '" "' + str(PRC_2_scaled) + '" 25')   
    os.system('python realistic_FOV_aep_tissot_multiple_colors_2ndeye_aolp_lines.py ' +args.input + ' aolp_2ndeye_' + str(rotation_angle) + '_lines.png "' + str(azimuth_deg_list) + '" "' + str(elevation_deg_list) + '" "' + str(aolp_2_lines) + '" "' + str(dolp_2) + '"')
    #os.system('python realistic_FOV_aep_tissot_multiple_colors_2ndeye_aolp_phimax.py ' + args.input + ' aolp_2ndeye_' + str(rotation_angle) + '.png "' + str(azimuth_deg_list) + '" "' + str(elevation_deg_list) + '" "' + str(aolp_2) + '" 25')
    os.system('python make_mask_transparent.py aolp_2ndeye_' + str(rotation_angle) + '.png aolp_2ndeye_' + str(rotation_angle) + '_transparent.png')
    os.system('python make_mask_transparent.py aolp_2ndeye_' + str(rotation_angle) + '_lines.png aolp_2ndeye_' + str(rotation_angle) + '_lines_transparent.png')
    os.system('rm aolp_2ndeye_' + str(rotation_angle) + '.png')
    os.system('rm aolp_2ndeye_' + str(rotation_angle) + '_lines.png')

    os.system('python image_superimposing.py aolp_2ndeye_' + str(rotation_angle) + '_transparent.png aolp_2ndeye_' + str(rotation_angle) + '_lines_transparent.png aolp_2ndeye_' + str(rotation_angle) + '_transparent.png')
    
    os.system('python image_superimposing.py aolp_1steye_' + str(rotation_angle) + '_background.png aolp_2ndeye_' + str(rotation_angle) + '_transparent.png aolp_' + str(rotation_angle) + '_background.png')
    os.system('rm aolp_2ndeye_' + str(rotation_angle) + '_transparent.png')
    os.system('rm aolp_2ndeye_' + str(rotation_angle) + '_lines_transparent.png')
    os.system('rm aolp_1steye_' + str(rotation_angle) + '_background.png')
    os.system('python circular_masking.py aolp_' + str(rotation_angle) + '_background.png aolp_' + str(rotation_angle) + '_background_transparent.png')
    os.system('rm aolp_' + str(rotation_angle) + '_background.png')
    # frame to import is aolp_' + str(rotation_angle) + '_background_transparent.png

    circmeans.append(circmean(np.array(aolp), weights=np.array(dolp)))
    circmeans_2.append(circmean(np.array(aolp_2), weights=np.array(dolp_2)))
    circmeans_botheyes.append(circmean(np.array(aolp_both_eyes), weights=np.array(dolp_both_eyes)))

# Calculate the polar histogram for circmeans list
N = 59
aop_hist = np.histogram(circmeans, bins=N, range=[-np.pi, np.pi])

bottom = 0
max_height = 1102

theta = np.linspace(-np.pi, np.pi, N, endpoint=False)
radii = aop_hist[0]
width = 0.1 * (2 * np.pi) / N

# Plot the polar histogram
fig = plt.figure()
ax = fig.add_subplot(111, polar=True)
bars = ax.bar(theta, radii, width=width, bottom=bottom)
ax.set_rlabel_position(270)

# Use custom colors and opacity
for r, bar in zip(radii, bars):
    bar.set_facecolor('red')
    
# Set title with rotation angle
ax.set_title('aolp_left_eye_ommatidial_circular_means'.format(rotation_angle))

ax.set_xlabel("Angle of Polarization")

# Save the polar histogram
output_path = 'polar_histogram_1steye_circmeans.png'
fig.savefig(output_path)
plt.close(fig)

# Calculate the polar histogram for circmeans_2 list
N = 59
aop_hist = np.histogram(circmeans_2, bins=N, range=[-np.pi, np.pi])

bottom = 0
max_height = 1102

theta = np.linspace(-np.pi, np.pi, N, endpoint=False)
radii = aop_hist[0]
width = 0.1 * (2 * np.pi) / N

# Plot the polar histogram
fig = plt.figure()
ax = fig.add_subplot(111, polar=True)
bars = ax.bar(theta, radii, width=width, bottom=bottom)
ax.set_rlabel_position(270)

# Use custom colors and opacity
for r, bar in zip(radii, bars):
    bar.set_facecolor('red')
    
# Set title with rotation angle
ax.set_title('aolp_right_eye_ommatidial_circular_means'.format(rotation_angle))

ax.set_xlabel("Angle of Polarization")

# Save the polar histogram
output_path = 'polar_histogram_2ndeye_circmeans.png'
fig.savefig(output_path)
plt.close(fig)


# Calculate the polar histogram for circmeans both eyes
N = 59
aop_hist = np.histogram(circmeans_botheyes, bins=N, range=[-np.pi, np.pi])

bottom = 0
max_height = 1102

theta = np.linspace(-np.pi, np.pi, N, endpoint=False)
radii = aop_hist[0]
width = 0.1 * (2 * np.pi) / N

# Plot the polar histogram
fig = plt.figure()
ax = fig.add_subplot(111, polar=True)
bars = ax.bar(theta, radii, width=width, bottom=bottom)
ax.set_rlabel_position(270)

# Use custom colors and opacity
for r, bar in zip(radii, bars):
    bar.set_facecolor('red')
    
# Set title with rotation angle
ax.set_title('aolp_both_eyes_ommatidial_circular_means')

ax.set_xlabel("Angle of Polarization")

# Save the polar histogram
output_path = 'polar_histogram_both_eyes_circmeans.png'
fig.savefig(output_path)
plt.close(fig)


    
