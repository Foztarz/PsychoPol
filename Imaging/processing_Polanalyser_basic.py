# -*- coding: utf-8 -*-
"""
Created on Tue Mar 30 11:15:13 2021
# Details ---------------------------------------------------------------
#       AUTHOR:	James Foster        DATE: 2021 03 30
#     MODIFIED:	James Foster        DATE: 2021 03 30
#
#  DESCRIPTION: Loads image capture exported from Lucid's Arena SDK. Splits the 
#               image into different polarization channels based on Bayer mask
#               for a Sony IMX264MZR sensor. Calculates Stokes paramaters and
#               plots the degree of linear polarization (DoLP), angle of linear
#               polarization (AoLP), intensity, and AoLP scaled in brightness
#               by DoLP.
#               
#      OUTPUTS: Images as bitmap (png).
#
#	   CHANGES: -
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
#- Test run  
#- Comment  
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

imfile = askopenfilename()
img_raw  = cv2.imread(imfile,0)#0 means greyscale

img_demosaiced = pa.demosaicing(img_raw)

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

plt.imshow(img_intensity)
plt.imshow(img_DoLP, cmap = 'jet')
plt.imshow(img_AoLP, cmap = 'hsv')

img_AoLP_cmapped = pa.applyColorToAoLP(img_AoLP, value=img_DoLP)

plt.imshow(img_AoLP_cmapped)

img_DoLP_col = cm.jet(img_DoLP)
plt.imshow(img_DoLP_col.astype(np.float64))

img_AoLP_col = cm.hsv(img_AoLP/np.pi)
plt.imshow(img_AoLP_col.astype(np.float64))

img_DoLP_col_inv = cv2.cvtColor(img_DoLP_col.astype(np.float32), cv2.COLOR_RGB2BGR)
# plt.imshow(img_DoLP_col_inv.astype(np.float64))
img_AoLP_col_inv = cv2.cvtColor(img_AoLP_col.astype(np.float32), cv2.COLOR_RGB2BGR)
# plt.imshow(img_AoLP_col_inv.astype(np.float64))

fln = os.path.basename(imfile)[:-5]#crop the file type
cv2.imwrite(os.path.dirname(imfile)+'/Int_'+fln+".png",img_intensity.astype(np.float64))#np.uint8))
cv2.imwrite(os.path.dirname(imfile)+'/DoLP_'+fln+".png",img_DoLP_col_inv.astype(np.float64)*255)
cv2.imwrite(os.path.dirname(imfile)+'/AoLP_'+fln+".png",img_AoLP_col_inv.astype(np.float64)*255)
cv2.imwrite(os.path.dirname(imfile)+'/PolBright_'+fln+".png",img_AoLP_cmapped)


img_AoLP_colesque = pa.applyColorToAoLP(img_AoLP, value= img_intensity/255, saturation = img_DoLP)
plt.imshow(img_AoLP_colesque)
img_AoLP_colesque_inv = cv2.cvtColor(img_AoLP_colesque.astype(np.float32), cv2.COLOR_RGB2BGR)
#seems to work differently on Windows?
if os.name == 'nt' :
    cv2.imwrite(os.path.dirname(imfile)+'/PolColesque_'+fln+".png",img_AoLP_colesque)
else :
    cv2.imwrite(os.path.dirname(imfile)+'/PolColesque_'+fln+".png",img_AoLP_colesque_inv)
