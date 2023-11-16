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
from PIL import Image
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
#print(img_mid.max()) # this is 255
img_mid_over = np.where(img_mid > np.round(256*(1-edge_lim/100))-1)
hist_mid = plt.hist(img_mid.ravel(), 256, [0,256])
#print(np.shape(img_mid_over))
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
#print(ind_mid.size)
#plt.imshow(img_HDR, cmap = 'gray')

# histogram HDR
img_HDR_val = img_HDR.ravel()
fig = plt.figure()
ax = fig.add_subplot(111)
HDR_histogram = np.histogram(img_HDR_val, bins = np.uint8(1e3), 
                             range = [0, 
                                      np.nanmax(img_HDR_val)])
ax.set_xscale('log')
min_pos = np.min(np.where(HDR_histogram[0][1:-1] > 0))
zero_pos = HDR_histogram[1][min_pos]/1.1
brs = ax.bar(x =  np.append(zero_pos, HDR_histogram[1][1:-1]), 
             height = HDR_histogram[0],
             width= 0.5*np.log(np.append([1.3], 
                                         HDR_histogram[1][1:-1])), 
             ec="k", align="edge", color = ['tab:red'] + ['tab:blue'] * len(HDR_histogram[1][1:-1]) )
ax.set_title('HDR image')
ax.set_xlim([zero_pos/1.2, np.max(HDR_histogram[1][1:])*1.2] )
ax.set_xlabel("log10 pixel byte values / s")
ax.set_ylabel("Frequency")
fig.savefig( os.path.dirname(imfile)+ '/HDR_histogram.pdf')

# histogram img_mid
img_mid_val = img_mid.ravel()
fig = plt.figure()
ax = fig.add_subplot(111)
img_mid_histogram = np.histogram(img_mid_val, bins = np.uint8(1e3), 
                             range = [0, 
                                      np.nanmax(img_mid_val)])
ax.set_xscale('log')
min_pos = np.min(np.where(img_mid_histogram[0][1:-1] > 0))
zero_pos = img_mid_histogram[1][min_pos]/1.1
brs = ax.bar(x =  np.append(zero_pos, img_mid_histogram[1][1:-1]), 
             height = img_mid_histogram[0],
             width= 0.5*np.log(np.append([1.3], 
                                         img_mid_histogram[1][1:-1])), 
             ec="k", align="edge", color = ['tab:red'] + ['tab:blue'] * len(img_mid_histogram[1][1:-1]) )
ax.set_title('img_mid image')
ax.set_xlim([zero_pos/1.2, np.max(img_mid_histogram[1][1:])*1.2] )
ax.set_xlabel("log10 pixel byte values / s")
ax.set_ylabel("Frequency")
fig.savefig( os.path.dirname(imfile)+ '/img_mid_histogram.pdf')

# Display the final HDR image using cv2.imshow()
cv2.imshow('Final HDR Image', img_HDR.astype(np.float64)/img_HDR.max()) # save this image when it prompts, the cv2.imwrite() doesn't work properly.
# Wait for a key press and then close the window
cv2.waitKey(0)
cv2.destroyAllWindows()
#plt.show()

