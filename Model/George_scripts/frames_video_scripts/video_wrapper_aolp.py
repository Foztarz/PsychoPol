## usage: python video_wrapper.py -i HDR.png -dmc img_000_cropped.png img_045_cropped.png img_090_cropped.png img_135_cropped.png -c coordinates.txt -o output.mp4


import os
import sys
import argparse
import cv2
import numpy as np
import subprocess
import math

parser = argparse.ArgumentParser()
parser.add_argument("-i", "--input", required=True, help="INPUT must be the input sky image. It must be square with transparent edges. (required)")
parser.add_argument("-dmc", "--demosaiced", nargs='+', required=True, help="DEMOSAICED must be the 4 input CROPPED demosaiced images (000,045,090,135). (required)")
parser.add_argument("-c", "--coordinates", required=True, help="COORDINATES must be a text file with two columns, tab-separated. Each line contains coordinates (azimuth, elevation) for the FOV of one ommatidium. (required)")
#parser.add_argument("-o", "--output", required=True, help="OUTPUT must be the output video file (.mp4). (required)")
args=parser.parse_args()

#video = cv2.VideoWriter(output_video_path, cv2.VideoWriter_fourcc(*'mp4v'), 5, frame_size)

azimuth_deg_list = []
elevation_deg_list = []

with open(args.coordinates, 'r') as crd:
    for line in crd:
        x=line.split('\t')
        azimuth_deg_list.append(float(x[0]))
        elevation_deg_list.append(float(x[1].strip()))
#print(azimuth_deg_list)
#print(elevation_deg_list)

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
        aolp = [ math.atan2(x3, y3) / 2 for x3, y3 in zip(S2, S1)] # this is opposite than excel atan2, in excel it should be zip(S2,S1)

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
        aolp_2 = [ math.atan2(x3, y3) / 2 for x3, y3 in zip(S2_2, S1_2)] # this is opposite than excel atan2, in excel it should be zip(S2,S1)
    
    os.system('python realistic_FOV_aep_tissot_multiple_colors_aolp.py ' + args.input + ' aolp_1steye_' + str(rotation_angle) + '.png "' + str(azimuth_deg_list) + '" "' + str(elevation_deg_list) + '" "' + str(aolp) + '" 25')
    os.system('python make_mask_transparent.py aolp_1steye_' + str(rotation_angle) + '.png aolp_1steye_' + str(rotation_angle) + '_transparent.png')
    os.system('rm aolp_1steye_' + str(rotation_angle) + '.png')
    
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
    os.system('rm 1steye_' + str(rotation_angle) + '_background.png')
    os.system('rm aolp_1steye_' + str(rotation_angle) + '_transparent.png')
    

    os.system('python realistic_FOV_aep_tissot_multiple_colors_2ndeye_aolp.py ' + args.input + ' aolp_2ndeye_' + str(rotation_angle) + '.png "' + str(azimuth_deg_list) + '" "' + str(elevation_deg_list) + '" "' + str(aolp_2) + '" 25')
    os.system('python make_mask_transparent.py aolp_2ndeye_' + str(rotation_angle) + '.png aolp_2ndeye_' + str(rotation_angle) + '_transparent.png')
    os.system('rm aolp_2ndeye_' + str(rotation_angle) + '.png')

    os.system('python image_superimposing.py aolp_1steye_' + str(rotation_angle) + '_background.png aolp_2ndeye_' + str(rotation_angle) + '_transparent.png aolp_' + str(rotation_angle) + '_background.png')
    os.system('rm aolp_2ndeye_' + str(rotation_angle) + '_transparent.png')
    os.system('rm aolp_1steye_' + str(rotation_angle) + '_background.png')
    os.system('python circular_masking.py aolp_' + str(rotation_angle) + '_background.png aolp_' + str(rotation_angle) + '_background_transparent.png')
    os.system('rm aolp_' + str(rotation_angle) + '_background.png')
    # frame to import is aolp_' + str(rotation_angle) + '_background_transparent.png
    
    #print(S0_2)
    #print(dolp)
