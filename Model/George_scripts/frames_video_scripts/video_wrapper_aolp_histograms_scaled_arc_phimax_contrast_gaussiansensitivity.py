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
from astropy.units import Quantity
from scipy.interpolate import make_interp_spline

parser = argparse.ArgumentParser()
parser.add_argument("-i", "--input", required=True, help="INPUT must be the input sky image. It must be square with transparent edges. (required)")
parser.add_argument("-dmc", "--demosaiced", nargs='+', required=True, help="DEMOSAICED must be the 4 input CROPPED demosaiced images (000,045,090,135). (required)")
parser.add_argument("-c", "--coordinates", required=True, help="COORDINATES must be a text file with two columns, tab-separated. Each line contains coordinates (azimuth, elevation) for the FOV of one ommatidium. (required)")
parser.add_argument("-saz", "--solarazimuth", required=True, help="SOLARAZIMUTH must be the true solar azimuth in the image (including magnetic declination). (required)")
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

circmeans = []
circmeans_2 = []
circmeans_botheyes = []
all_PRC_values = []
all_PRC_2_values = []
rotation_angles = []
for rotation_angle in range(0, 360, 5):
    solar_azimuth = float(args.solarazimuth) + rotation_angle - 180 # subtracting 180deg to have 0deg down on the image
    if solar_azimuth < 360:
        rotation_angles.append(solar_azimuth)
    else:
        rotation_angles.append(solar_azimuth -360)
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
        with subprocess.Popen(str('python realistic_FOV_aep_tissot_multiple_omm_intensities_forstokes_gaussian_sensitivity.py ' + img_dmc + ' ' + args.coordinates + ' 25 ' + str(rotation_angle)), shell=True, stdout=subprocess.PIPE, text=True) as process:
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
    aolp = [ np.mod(((math.atan2(x3, y3) / 2) + np.radians(rotation_angle)), np.pi) / np.pi for x3, y3 in zip(S2, S1)] # divide by pi for colors, mod for values between 0 and 180
    aolp_hist = [ np.mod(((-math.atan2(x3, y3) / 2) + np.radians(rotation_angle) + np.radians(90)), np.pi) for x3, y3 in zip(S2, S1)] 
    aolp_lines = [ (math.atan2(x3, y3) / 2) + np.radians(rotation_angle) for x3, y3 in zip(S2, S1)]
    aolp_circmeans = [ np.mod(((-math.atan2(x3, y3) / 2) + np.radians(90)), np.pi) for x3, y3 in zip(S2, S1)] # no rotation angle for circmeans
    
    # Calculate the polar histogram for aolp list
    N = 60 # change this to 60 (from 59) to have a bin for every 5deg
    aop_hist = np.histogram(aolp_hist, bins=N, range=[-np.pi, np.pi])

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
        with subprocess.Popen(str('python realistic_FOV_aep_tissot_multiple_omm_intensities_forstokes_2ndeye_gaussian_sensitivity.py ' + img_dmc + ' ' + args.coordinates + ' 25 ' + str(rotation_angle)), shell=True, stdout=subprocess.PIPE, text=True) as process:
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
    aolp_2 = [ np.mod(((math.atan2(x3, y3) / 2) + np.radians(rotation_angle)), np.pi) / np.pi for x3, y3 in zip(S2_2, S1_2)] # divide by pi for colors , mod for values between 0 and 180
    aolp_2_hist = [ np.mod(((-math.atan2(x3, y3) / 2) + np.radians(rotation_angle) + np.radians(90)), np.pi) for x3, y3 in zip(S2_2, S1_2)] 
    aolp_2_lines = [ (math.atan2(x3, y3) / 2) + np.radians(rotation_angle) for x3, y3 in zip(S2_2, S1_2)]
    aolp_2_circmeans = [ np.mod(((-math.atan2(x3, y3) / 2) + np.radians(90)), np.pi) for x3, y3 in zip(S2_2, S1_2)] # no rotation angle for circmeans
    
    # Calculate the polar histogram for aolp_2 list
    N = 60 # change this to 60 (from 59) to have a bin for every 5deg
    aop_hist = np.histogram(aolp_2_hist, bins=N, range=[-np.pi, np.pi])

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

    aolp_both_eyes = aolp_circmeans + aolp_2_circmeans
    dolp_both_eyes = dolp + dolp_2
    
    # Calculate the polar histogram for both eyes
    N = 60 # change this to 60 (from 59) to have a bin for every 5deg
    aop_hist_1 = np.histogram(aolp_hist, bins=N, range=[-np.pi, np.pi])

    # Calculate the polar histogram for the second set of AOLP values
    aop_hist_2 = np.histogram(aolp_2_hist, bins=N, range=[-np.pi, np.pi])

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

    
    os.system('python realistic_FOV_aep_tissot_multiple_colors_aolp_phimax_arcs.py ' + args.input + ' aolp_1steye_' + str(rotation_angle) + '.png "' + str(azimuth_deg_list) + '" "' + str(elevation_deg_list) + '" "' + str(aolp) + '" 25')

    with open("azimuth_elevation_arc_intercepts_data.csv", 'r') as phimax_values:
        for line in phimax_values:
            x = line.split('\t')
            phimax_list.append(float(x[3]))
            phimax_2_list.append(float(x[4]))
            
    # for the first eye
    for aolp_value, dolp_value, phimax_value, phimax_2_value in zip(aolp_lines, dolp, phimax_list, phimax_2_list):
        S_list.append(float(1 + ((dolp_value*(Sp - 1)) / (Sp + 1)) * np.cos(2*aolp_value - 2*np.radians(phimax_value))))
        S2_list.append(float(1 + ((dolp_value*(Sp - 1)) / (Sp + 1)) * np.cos(2*aolp_value - 2*np.radians(phimax_2_value))))
    for S_value, S_2_value in zip(S_list, S2_list):
        PRC.append(float(np.log(S_value / S_2_value)))
    PRC_scaled = np.interp(PRC, (-np.log(Sp*0.7),np.log(Sp*0.7)), (0,1)) # we use the log here, derived from theoretical max ratio, multiplying by 0.7 which is typical max DoLP in the sky
    PRC_scaled = PRC_scaled.tolist()
    all_PRC_values.append(PRC)

    # for the second eye
    for aolp_value, dolp_value, phimax_value, phimax_2_value in zip(aolp_2_lines, dolp_2, phimax_list, phimax_2_list):
        S_2_list.append(float(1 + ((dolp_value*(Sp - 1)) / (Sp + 1)) * np.cos(2*aolp_value - 2*np.radians(phimax_value))))
        S2_2_list.append(float(1 + ((dolp_value*(Sp - 1)) / (Sp + 1)) * np.cos(2*aolp_value - 2*np.radians(phimax_2_value))))
    for S_value, S_2_value in zip(S_2_list, S2_2_list):
        PRC_2.append(float(np.log(S_value / S_2_value)))
    PRC_2_scaled = np.interp(PRC_2, (-np.log(Sp*0.7),np.log(Sp*0.7)), (0,1)) # we use the log here, derived from theoretical max ratio, multiplying by 0.7 which is typical max DoLP in the sky
    PRC_2_scaled = PRC_2_scaled.tolist()
    all_PRC_2_values.append(PRC_2)
    
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

    circmeans.append(circmean(np.array(aolp_circmeans), weights=np.array(dolp)))
    circmeans_2.append(circmean(np.array(aolp_2_circmeans), weights=np.array(dolp_2)))
    circmeans_botheyes.append(circmean(np.array(aolp_both_eyes), weights=np.array(dolp_both_eyes)))

# Calculate the polar histogram for circmeans list
N = 60 # change this to 60 (from 59) to have a bin for every 5deg
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
N = 60 # change this to 60 (from 59) to have a bin for every 5deg
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
N = 60 # change this to 60 (from 59) to have a bin for every 5deg
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

# Plot and save for ommatidia pairs (PRC values)
for i in range(len(all_PRC_values[0])):
    plt.figure()
    
    # Plot for right eye (PRC values across all solar azimuths)
    plt.plot(sorted_rotation_angles, [lst[i] for lst in sorted_all_PRC_values], label=f'right eye - ommatidium {i+1}', c='blue')

    # Plot for left eye (PRC values across all solar azimuths)
    plt.plot(sorted_rotation_angles, [lst[i] for lst in sorted_all_PRC_2_values], label=f'left eye - ommatidium {i+1}', c='red')
    
    # Plot the difference between right and left eye values
    difference_PRC_values = [lst1[i] - lst2[i] for lst1, lst2 in zip(sorted_all_PRC_values, sorted_all_PRC_2_values)]
    plt.plot(sorted_rotation_angles, difference_PRC_values, label=f'PRC_difference - ommatidium {i+1}', c='green')

    plt.title(f'Graph for ommatidium {i+1}')
    plt.xlabel('solar azimuth (degrees)')
    plt.ylabel('PRC value')
    plt.legend()
    # Set x-axis ticks at every ten degrees
    plt.xticks(range(0, 361, 30))
    # Save the figure in the current working directory
    figure_filename = f'ommatidium_{i+1}.png'
    plt.savefig(figure_filename)
    plt.close()

# Create a single graph for all right eye ommatidia
plt.figure()
for i in range(len(all_PRC_values[0])):
    plt.plot(sorted_rotation_angles, [lst[i] for lst in sorted_all_PRC_values], c = 'blue')

plt.title('All Right Eye Ommatidia')
plt.xlabel('solar azimuth (degrees)')
plt.ylabel('PRC value')
# Set x-axis ticks at every ten degrees
plt.xticks(range(0, 361, 30))
plt.savefig('all_right_eye_ommatidia.png')
plt.close()

# Create a single graph for all left eye ommatidia
plt.figure()
for i in range(len(all_PRC_2_values[0])):
    plt.plot(sorted_rotation_angles, [lst[i] for lst in sorted_all_PRC_2_values], c = 'red')

plt.title('All Left Eye Ommatidia')
plt.xlabel('solar azimuth (degrees)')
plt.ylabel('PRC value')
plt.savefig('all_left_eye_ommatidia.png')
plt.close()

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

    
