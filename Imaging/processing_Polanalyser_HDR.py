# -*- coding: utf-8 -*-
"""
Created on Thu May  6 17:51:26 2021

# Details ---------------------------------------------------------------
#       AUTHOR:	James Foster        DATE: 2021 03 30
#     MODIFIED:	James Foster        DATE: 2021 06 23
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
#      OUTPUTS: Images as bitmap (pdf).
#
#	   CHANGES: -double saturated versions
#	            -sigmoid scaling
#	            -save DoLP histogram
#	            -mask DoLP
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
# nonzero = img_HDR.ravel()[img_HDR.ravel()>0]
# fig.close()
#create mask
# msk = np.zeros(img_HDR.shape[:2], np.uint8)
# msk[np.where(img_HDR > 0)] = 255
# plt.imshow(msk)
# nn = cv2.calcHist([nonzero.astype('uint8')], channels = [0], mask = msk, histSize = [1000], ranges = [0,75])
# plt.plot(nn)
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

"""
## create mask for lens shape
"""
if lens_type == 'fisheye' :
    lens_radius = np.float64(img_DoLP.shape[1])/2 * (1 - 80/256)
    msk = np.empty(img_DoLP.shape[:2], np.float64)
    msk[:] = np.nan
    ctr = (np.float64(img_DoLP.shape[:2])+0)/2
    #these loops are VERY slow
    # im_coords = [[row, col] for row in range(0, img_DoLP.shape[0] - 1) for col in range(img_DoLP.shape[1] - 1)]
    # rowcol_distance2= [(np.square(i[0]), np.square(i[1]))  for i in (np.float64(im_coords)-ctr).tolist()]
    # ctr_distance = [(np.sqrt(i[0] + i[1]))  for i in rowcol_distance2]
    # lens_coords = [im_coords[i] for i in lens_ind]
    #instead, construct the coordinates using an array function
    im_coords = np.ones( np.append(2, img_DoLP.shape), dtype = np.int16)
    im_coords[0] = im_coords[0] * np.array([range(img_DoLP.shape[0])]).T
    im_coords[1] = im_coords[1] * np.array([range(img_DoLP.shape[1])])
    im_coords = np.hstack((im_coords[0].reshape(-1, 1),
                           im_coords[1].reshape(-1, 1)))
    #set up a function to map onto these coordinates
    Diag_dist  = lambda i: np.sqrt(np.square(i[0]) + np.square(i[1]))
    #perform this function on the difference between coordinates and the centre
    ctr_distance = np.array(list(map( Diag_dist,  
                                     np.float64(im_coords)-ctr
                                     )))
    #select the indices of pixel illuminated by the lens
    lens_ind = np.where(ctr_distance < lens_radius)[0].tolist()
    #select those coordinates
    lens_coords = im_coords[lens_ind]
    #set those coordinates to one
    msk[[i[0] for i in lens_coords], [i[1] for i in lens_coords]] = 1
else:
    msk = np.ones(img_DoLP.shape[:2], np.float64)
    
#apply the mask to the processed images   
img_DoLP_msk = img_DoLP.astype(np.float64) * msk
img_AoLP_msk = img_AoLP.astype(np.float64) * msk
img_HDR_msk = img_HDR.astype(np.float64) * msk
img_intensity_msk = img_intensity.astype(np.float64) * msk
# plt.imshow(img_intensity_msk)
# plt.imshow(img_HDR_msk)
# plt.imshow(img_DoLP_msk)
#plt.imshow(img_AoLP_msk)


"""
## make intensity histogram
"""
img_HDR_val = img_HDR_msk.ravel()
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
np.savetxt(os.path.dirname(imfile)+"/HDR_histogram.csv", 
          [HDR_histogram[0], HDR_histogram[1][:-1]] , 
           delimiter=',')

"""
## make DoLP histogram
"""
fig = plt.figure()
ax = fig.add_subplot(111)
DoLP_histogram = ax.hist(  img_DoLP_msk.ravel(), bins = [i/100 for i in range(0, 101, 1)])
DoLP_freq = DoLP_histogram[0].tolist()
DoLP_bins = DoLP_histogram[1].tolist()
DoLP_binc = [i + np.mean(np.diff(DoLP_bins))/2 for i in DoLP_bins[:(len(DoLP_bins)-1)]]
DoLP_bars = DoLP_histogram[2].get_children()
for i in range(0, len(DoLP_bars)):
    DoLP_bars[i].set_facecolor( plt.cm.jet(DoLP_binc[i]) )
    DoLP_bars[i].set_edgecolor( ((0.5,0.5,0.5,1.)) )
ax.set_title('Image Pixels')
ax.set_xlabel("Degree of Polarization")
ax.set_ylabel("Frequency")
fig.savefig( os.path.dirname(imfile)+ '/DoLP_histogram.pdf')
np.savetxt(os.path.dirname(imfile)+"/DoLP_histogram.csv", 
          [DoLP_freq, DoLP_binc] , 
           delimiter=',')


"""
## make AoLP histogram
"""
N = 59

aop_hist = np.histogram(img_AoLP_msk,bins=N,range = [0.001, np.pi]) 

bottom = 0
max_height = 1102

theta = np.linspace(0.0, np.pi, N, endpoint=False)
radii = aop_hist[0] #/ np.product(img_AoLP_msk.shape)
width = 0.1*(2*np.pi) / N
    # width = (2*np.pi) / N
fig = plt.figure()
ax = fig.add_subplot(111, polar=True)
# ax.set_theta_zero_location("N")
bars = ax.bar(theta, radii, width=width, bottom=bottom)
ax.set_rlabel_position(270)

# Use custom colors and opacity
for r, bar in zip(radii, bars):
    bar.set_facecolor('red')
    # bar.set_facecolor(plt.cm.jet(r / 10.))
    # bar.set_alpha(0.8)
ax.set_title('Image Pixels')
ax.set_xlabel("Angle of Polarization")
# ax.set_ylabel("Frequency")
fig.savefig( os.path.dirname(imfile)+ '/AoLP_histogram.pdf')
np.savetxt(os.path.dirname(imfile)+"/AoLP_histogram.csv", 
          [np.uint8(radii), np.uint8(np.round(theta* 180./np.pi))] , 
           delimiter=',')

"""
## make sigmoid scaling
"""

def  Scale_sigmoid(x, inflex = 0., width = 2., rang = 0.8):
    bx = (2*np.log((1/((1-rang)/2))-1)*(x-inflex))/width
    yy = 1/(1+np.exp(-(bx)))
    return(yy)


nonzero = img_intensity_msk[np.nonzero(img_intensity_msk)].ravel()
img_displ_int = Scale_sigmoid(img_intensity_msk, 
                              inflex= np.nanmedian(nonzero),
                              width = np.diff(np.nanquantile(nonzero, [(1-max_val)/2, 1-(1-max_val)/2])),
                              rang = max_val)

plt.imshow(img_displ_int,   cmap = 'gray', vmin = 0, vmax = 1)

# plt.imshow(img_intensity)
plt.imshow(img_DoLP_msk, cmap = 'jet', vmin=0, vmax=1)
plt.imshow(img_AoLP_msk, cmap = 'hsv')

img_AoLP_cmapped = pa.applyColorToAoLP(img_AoLP_msk, value=img_DoLP_msk)

plt.imshow(img_AoLP_cmapped)

img_DoLP_col = cm.jet(img_DoLP_msk)
plt.imshow(img_DoLP_col.astype(np.float64))

img_AoLP_col = cm.hsv(img_AoLP_msk/np.pi)
plt.imshow(img_AoLP_col.astype(np.float64))

img_DoLP_col_inv = cv2.cvtColor(img_DoLP_col.astype(np.float32), cv2.COLOR_RGB2BGR)
# plt.imshow(imgs_DoLP_col_inv.astype(np.float64))
img_AoLP_col_inv = cv2.cvtColor(img_AoLP_col.astype(np.float32), cv2.COLOR_RGB2BGR)
# plt.imshow(imgs_AoLP_col_inv.astype(np.float64))

fln = os.path.basename(os.path.dirname(imfile))#crop the file type
# cv2.imwrite(os.path.dirname(imfile)+'/HDR_Int_'+fln+".png",255*img_intensity.astype(np.float64)/np.max(img_intensity))#np.uint8))
# cv2.imwrite(os.path.dirname(imfile)+'/HDR_Int_'+fln+".png",255*img_intensity.astype(np.float64)/np.quantile(img_intensity,max_val))#np.uint8))
cv2.imwrite(os.path.dirname(imfile)+'/HDR_Int_'+fln+".png",img_displ_int.astype(np.float64)*255)#np.uint8))
cv2.imwrite(os.path.dirname(imfile)+'/HDR_DoLP_'+fln+".png",img_DoLP_col_inv.astype(np.float64)*255)
cv2.imwrite(os.path.dirname(imfile)+'/HDR_AoLP_'+fln+".png",img_AoLP_col_inv.astype(np.float64)*255)
# cv2.imwrite(os.path.dirname(imfile)+'/HDR_PolBright_'+fln+".png",img_AoLP_cmapped)


# img_AoLP_colesque = pa.applyColorToAoLP(img_AoLP, value= img_intensity/np.max(img_intensity), saturation = img_DoLP)
# img_AoLP_colesque = pa.applyColorToAoLP(img_AoLP, value= img_intensity/np.quantile(img_intensity,max_val), saturation = img_DoLP)
# img_AoLP_colesque = pa.applyColorToAoLP(img_AoLP_msk, value= img_displ_int/np.quantile(img_displ_int,max_val), saturation = img_DoLP_msk)
img_AoLP_colesque = pa.applyColorToAoLP(img_AoLP_msk, value= img_displ_int, saturation = img_DoLP_msk)
plt.imshow(img_AoLP_colesque)
img_AoLP_colesque_inv = cv2.cvtColor(img_AoLP_colesque.astype(np.float32), cv2.COLOR_RGB2BGR)
#seems to work differently on Windows?
# if os.name == 'nt' :
#     cv2.imwrite(os.path.dirname(imfile)+'/HDR_PolColesque_'+fln+".png",img_AoLP_colesque)
# else :
#     cv2.imwrite(os.path.dirname(imfile)+'/HDR_PolColesque_'+fln+".png",img_AoLP_colesque_inv)

# img_AoLP_Supercolesque = pa.applyColorToAoLP(img_AoLP, value= (img_intensity**gamma_corr)/np.quantile(img_intensity,max_val), saturation = img_DoLP*2)
img_AoLP_Supercolesque = pa.applyColorToAoLP(img_AoLP_msk, value= img_displ_int, saturation = img_DoLP_msk*2)
plt.imshow(img_AoLP_Supercolesque)
img_AoLP_Supercolesque_inv = cv2.cvtColor(img_AoLP_Supercolesque.astype(np.float32), cv2.COLOR_RGB2BGR)
parms = list()
parms.append(cv2.IMWRITE_PNG_COMPRESSION)
parms.append(0)
#seems to work differently on Windows?
if os.name == 'nt' :
    cv2.imwrite(os.path.dirname(imfile)+'/HDR_PolSuperColesque_'+fln+".png",img_AoLP_Supercolesque,
                params = parms)
else :
    cv2.imwrite(os.path.dirname(imfile)+'/HDR_PolSuperColesque_'+fln+".png",img_AoLP_Supercolesque_inv,
                params = parms)

"""
#Save as csv?
"""
#Downsample x 2^-8
AoLP = np.asarray(cv2.pyrDown(cv2.pyrDown(cv2.pyrDown(img_AoLP))),dtype=np.float64)
DoLP = np.asarray(cv2.pyrDown(cv2.pyrDown(cv2.pyrDown(img_DoLP))),dtype=np.float64)
np.savetxt(os.path.dirname(imfile)+"/AoLP_pixels.csv", AoLP, delimiter=',')
np.savetxt(os.path.dirname(imfile)+"/DoLP_pixels.csv", DoLP, delimiter=',')
