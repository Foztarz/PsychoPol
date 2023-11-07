## usage: python HDR.py
## you select then one file from the folder with the .tiff files and save the image that prompts

import numpy as np
from matplotlib import pyplot as plt
from matplotlib.pylab import cm
import cv2
import sys
from tkinter.filedialog import askopenfilename
import os
import warnings
"""
## Input params
"""
fileformat = '.tiff'
expos_type = 'name'#exposure is '--######us'
edge_lim = 10#% bottom and top 10% replaced
# gamma_corr = 1.0#gamma correction for final image#N.B. sigmoid scaling now used
#Quantile to fit within display sigmoid
max_val = 0.999#0.95 recommended if sun or moon visible, otherwise 1.0 or 0.99
#lens type, fisheye or zoom
lens_type = 'fisheye'

"""
## Find files
"""
#ask user for a file
imfile = askopenfilename()
#find all files in that folder
imdir = os.listdir(os.path.dirname(imfile))
#find just the tiffs
tiffs = np.take(imdir, np.where( [ff.endswith(fileformat) for ff in imdir] ) )
#WIP, this indexing was a nightmare!

"""
## Find exposure values
"""
def Extract_exposures(label):
    extrExp = {
        'name': [np.float64(ff.split('--')[1].split('us')[0])/1000 for ff in tiffs[0]],
        'exif': warnings.warn('not implemented')
        }
    return(extrExp.get(label, 'Exposure type not implemented'))
exposures = Extract_exposures(expos_type)

"""
## Read in files as list
"""

imgs_raw  = [cv2.imread(os.path.dirname(imfile)+'/'+imfl,0) for imfl in tiffs[0]]#0 means greyscale
"""
## Check for over/underexposed pixels in middle exposure
"""
img_mid = imgs_raw[np.where(exposures == np.median(exposures))[0][0]]
img_mid_over = np.where(img_mid > np.round(256*(1-edge_lim/100))-1)
img_mid_under = np.where(img_mid < np.round(256*(edge_lim/100))-1)
"""
## Convert to units of pixel-byte-value/second
"""
imgs_bytes_s = imgs_raw
for ii in range(len(imgs_raw)):
    imgs_bytes_s[ii] = np.float64(imgs_raw[ii]) / exposures[ii]
    
"""
## Construct single HDR image
"""
ind_mid = np.where(exposures == np.median(exposures))[0][0]
ind_max = np.where(exposures == np.max(exposures))[0][0]
ind_min = np.where(exposures == np.min(exposures))[0][0]
img_HDR = imgs_bytes_s[ind_mid]

img_HDR[(img_mid_over[0],img_mid_over[1])] = imgs_bytes_s[ind_min][(img_mid_over[0],img_mid_over[1])]

img_HDR[(img_mid_under[0],img_mid_under[1])] = imgs_bytes_s[ind_max][(img_mid_under[0],img_mid_under[1])]

# Display the final HDR image using cv2.imshow()
cv2.imshow('Final HDR Image', img_HDR) # save this image when it prompts, the cv2.imwrite() doesn't work properly.

# Wait for a key press and then close the window
cv2.waitKey(0)
cv2.destroyAllWindows()


