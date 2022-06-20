# -*- coding: utf-8 -*-
"""
Created on Thu May  6 17:51:26 2021

# Details ---------------------------------------------------------------
#       AUTHOR:	James Foster        DATE: 2021 03 30
#     MODIFIED:	James Foster        DATE: 2021 06 18
#
#  DESCRIPTION: Loads images capture exported from Lucid's Arena SDK. These 
#               should consist of a bracket of images separated by 1–2EV. 
#               Exposure length is extracted from the filename and used to 
#               to convert the images to their relative values. Overexposed or
#               underexposed pixels in the middle exposure are then replaced.
#               It then splits the final image into different polarization 
#               channels based on the Bayer mask for a Sony IMX264MZR sensor, 
#               calculates Stokes paramaters and plots the degree of linear 
#               polarization (DoLP), angle of linear polarization (AoLP), 
#               intensity, and AoLP scaled in brightness by linearised (or gamma-
#               corrected) brightness and in saturation by DoLP.
#               
#      OUTPUTS: Images as bitmap (png).
#
#	   CHANGES: -double saturated versions
#
#   REFERENCES: Foster J.J., Temple S.E., How M.J., Daly I.M., Sharkey C.R.,
#               Wilby D., Roberts N.W., (2018)
#               Polarisation vision: overcoming challenges of working with a 
#               property of light we barely see. 
#               The Science of Nature 105, 27–27. 
#               https://doi.org/10.1007/s00114-018-1551-3
#               
#               Polanalyser by Ryota Maeda
#               https://github.com/elerac/polanalyser/wiki
#               https://github.com/elerac/polanalyser#polarization-demosaicing
#               
#               Sony Polarization Image Sensor range
#               https://www.sony-semicon.co.jp/products/common/pdf/IMX250_264_253MZR_MYR_Flyer_en.pdf
# 
#TODO   
#- Test run  +
#- Comment  
#- HDR      +
#- Image filtering
#- Dark subtraction
#- Neaten up          

"""
# conda install git

# pip install git+https://github.com/elerac/polanalyser
# pip install numba

import numpy as np
from matplotlib import pyplot as plt
from matplotlib.pylab import cm
import cv2

import polanalyser as pa

from tkinter.filedialog import askopenfilename
import os
# import re
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
hist_mid = plt.hist(img_mid.ravel(), 256, [0,256])
img_mid_over = np.where(img_mid > np.round(256*(1-edge_lim/100))-1)
img_mid_under = np.where(img_mid < np.round(256*(edge_lim/100))-1)
# over_r, over_c = np.where(img_mid > np.round(256*(1-edge_lim/100))-1)
# under_r, under_c = np.where(img_mid > np.round(256*(edge_lim/100))-1)
#use these indices later to replace 
        # img_temp = img_mid
        # img_temp[img_mid_under] = 255
        # img_temp[img_mid_over] = 0
        # plt.imshow(img_temp)
        # img_temp[img_mid_under] = imgs_raw[1][img_mid_under]
        # img_temp[img_mid_over] = imgs_raw[1][img_mid_under]
        # plt.imshow(img_temp)

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
plt.imshow(img_HDR)

img_HDR[(img_mid_over[0],img_mid_over[1])] = imgs_bytes_s[ind_min][(img_mid_over[0],img_mid_over[1])]
plt.imshow(img_HDR)

img_HDR[(img_mid_under[0],img_mid_under[1])] = imgs_bytes_s[ind_max][(img_mid_under[0],img_mid_under[1])]
plt.imshow(img_HDR)
        # hist_HDR = plt.hist(img_HDR.ravel(), 256)
        # hist_HDR = plt.hist(np.log10(img_HDR.ravel()+1))
        # plt.imshow(img_HDR)
        # plt.imshow(np.log10(img_HDR+1))
        # plt.imshow(np.log10(np.float64(img_mid)+1))
#hist_HDR = plt.hist(np.log10(img_HDR.ravel()+1),1000)
nonzero = img_HDR.ravel()[img_HDR.ravel()>0]
fig = plt.figure()
ax = fig.add_subplot(111)
ax.hist(np.log10(nonzero), bins = 100)
ax.set_title('HDR image')
ax.set_xlabel("log10 pixel byte values / s")
ax.set_ylabel("Frequency")
fig.savefig( os.path.dirname(imfile)+ '/HDR_histogram.png')
# fig.close()

"""
## Process for polarization
"""

img_demosaiced = pa.demosaicing(img_HDR)

img_000, img_045, img_090, img_135 = cv2.split(img_demosaiced)

plt.imshow(img_000)

# Calculate the Stokes vector per-pixel
radians = np.array([0, np.pi/4, np.pi/2, np.pi*3/4])
img_stokes = pa.calcStokes(img_demosaiced, radians)

plt.imshow(img_stokes)

# Decompose the Stokes vector into its components
img_S0, img_S1, img_S2 = cv2.split(img_stokes)

# Convert the Stokes vector to Intensity, DoLP and AoLP
img_intensity = pa.cvtStokesToIntensity(img_stokes)
img_DoLP      = pa.cvtStokesToDoLP(img_stokes)
img_AoLP      = pa.cvtStokesToAoLP(img_stokes)

def  Scale_sigmoid(x, inflex = 0, width = 2, rang = 0.8):
    bx = (2*np.log((1/((1-rang)/2))-1)*(x-inflex))/width
    yy = 1/(1+np.exp(-(bx)))
    return(yy)

img_displ_int = Scale_sigmoid(img_intensity, 
                              inflex= np.median(nonzero),
                              width = np.diff(np.quantile(nonzero, [(1-max_val)/2, 1-(1-max_val)/2])),
                              rang = max_val)

plt.imshow(img_displ_int,   cmap = 'gray', vmin = 0, vmax = 1)

# plt.imshow(img_intensity)
plt.imshow(img_DoLP, cmap = 'jet', vmin=0, vmax=1)
plt.imshow(img_AoLP, cmap = 'hsv')

img_AoLP_cmapped = pa.applyColorToAoLP(img_AoLP, value=img_DoLP)

plt.imshow(img_AoLP_cmapped)

img_DoLP_col = cm.jet(img_DoLP)
plt.imshow(img_DoLP_col.astype(np.float64))

img_AoLP_col = cm.hsv(img_AoLP/np.pi)
plt.imshow(img_AoLP_col.astype(np.float64))

img_DoLP_col_inv = cv2.cvtColor(img_DoLP_col.astype(np.float32), cv2.COLOR_RGB2BGR)
# plt.imshow(imgs_DoLP_col_inv.astype(np.float64))
img_AoLP_col_inv = cv2.cvtColor(img_AoLP_col.astype(np.float32), cv2.COLOR_RGB2BGR)
# plt.imshow(imgs_AoLP_col_inv.astype(np.float64))

fln = os.path.basename(os.path.dirname(imfile))#crop the file type
# cv2.imwrite(os.path.dirname(imfile)+'/HDR_Int_'+fln+".png",255*img_intensity.astype(np.float64)/np.max(img_intensity))#np.uint8))
# cv2.imwrite(os.path.dirname(imfile)+'/HDR_Int_'+fln+".png",255*img_intensity.astype(np.float64)/np.quantile(img_intensity,max_val))#np.uint8))
cv2.imwrite(os.path.dirname(imfile)+'/HDR_Int_'+fln+".png",255*img_displ_int.astype(np.float64))#np.uint8))
cv2.imwrite(os.path.dirname(imfile)+'/HDR_DoLP_'+fln+".png",img_DoLP_col_inv.astype(np.float64)*255)
cv2.imwrite(os.path.dirname(imfile)+'/HDR_AoLP_'+fln+".png",img_AoLP_col_inv.astype(np.float64)*255)
# cv2.imwrite(os.path.dirname(imfile)+'/HDR_PolBright_'+fln+".png",img_AoLP_cmapped)


# img_AoLP_colesque = pa.applyColorToAoLP(img_AoLP, value= img_intensity/np.max(img_intensity), saturation = img_DoLP)
# img_AoLP_colesque = pa.applyColorToAoLP(img_AoLP, value= img_intensity/np.quantile(img_intensity,max_val), saturation = img_DoLP)
img_AoLP_colesque = pa.applyColorToAoLP(img_AoLP, value= img_displ_int/np.quantile(img_displ_int,max_val), saturation = img_DoLP)
plt.imshow(img_AoLP_colesque)
img_AoLP_colesque_inv = cv2.cvtColor(img_AoLP_colesque.astype(np.float32), cv2.COLOR_RGB2BGR)
#seems to work differently on Windows?
# if os.name == 'nt' :
#     cv2.imwrite(os.path.dirname(imfile)+'/HDR_PolColesque_'+fln+".png",img_AoLP_colesque)
# else :
#     cv2.imwrite(os.path.dirname(imfile)+'/HDR_PolColesque_'+fln+".png",img_AoLP_colesque_inv)

# img_AoLP_Supercolesque = pa.applyColorToAoLP(img_AoLP, value= (img_intensity**gamma_corr)/np.quantile(img_intensity,max_val), saturation = img_DoLP*2)
img_AoLP_Supercolesque = pa.applyColorToAoLP(img_AoLP, value= img_displ_int, saturation = img_DoLP*2)
plt.imshow(img_AoLP_Supercolesque)
img_AoLP_Supercolesque_inv = cv2.cvtColor(img_AoLP_Supercolesque.astype(np.float32), cv2.COLOR_RGB2BGR)
#seems to work differently on Windows?
if os.name == 'nt' :
    cv2.imwrite(os.path.dirname(imfile)+'/HDR_PolSuperColesque_'+fln+".png",img_AoLP_Supercolesque)
else :
    cv2.imwrite(os.path.dirname(imfile)+'/HDR_PolSuperColesque_'+fln+".png",img_AoLP_Supercolesque_inv)

"""
#Save as csv?
"""
#Downsample x 2^-8
AoLP = np.asarray(cv2.pyrDown(cv2.pyrDown(cv2.pyrDown(img_AoLP))),dtype=np.float64)
DoLP = np.asarray(cv2.pyrDown(cv2.pyrDown(cv2.pyrDown(img_DoLP))),dtype=np.float64)
np.savetxt(os.path.dirname(imfile)+"/AoLP_pixels.csv", AoLP, delimiter=',')
np.savetxt(os.path.dirname(imfile)+"/DoLP_pixels.csv", DoLP, delimiter=',')
