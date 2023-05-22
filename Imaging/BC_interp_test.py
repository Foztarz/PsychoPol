# -*- coding: utf-8 -*-
"""
Created on Thu Dec  8 09:41:33 2022

@author: James Foster
"""
#adapted from:
#https://scipython.com/book/chapter-8-scipy/examples/two-dimensional-interpolation-with-scipyinterpolategriddata/

import numpy as np
from scipy.interpolate import griddata
import matplotlib.pyplot as plt
import cv2


from tkinter.filedialog import askopenfilename
import os
"""
## Find file
"""
#ask user for a file
imfile = askopenfilename()

"""
## Read in file
"""
img_orig  = cv2.imread(imfile,0)#0 means greyscale

"""
# Set up sample points
"""
#grid of image pixels
y = np.linspace(0,img_orig.shape[0]-1,img_orig.shape[0])
x =  np.linspace(0,img_orig.shape[1]-1,img_orig.shape[1])
X, Y = np.meshgrid(x,y)

# Choose npts random point from the discrete domain of our model function
subs = 500#only one in every 500 px kept, becomes slow with large numbers of samples
npts = int(img_orig.size/subs)#number of sample points
px, py = np.random.choice(x, npts), np.random.choice(y, npts)
#extract those image values
rr = img_orig[np.uint16(py), np.uint16(px)]

"""
# Plot the image and samples
"""
fig, ax = plt.subplots(nrows=2, ncols=2)
# Plot the original image
ax[0,0].imshow(img_orig, cmap = 'gray')
#plot the sampled values
ax[0,1].scatter(px, img_orig.shape[0]-py, 
            c = rr, 
            marker= '.', 
            cmap = 'gray',
            facecolor = 'k'
            )

"""
# Fit a grid across the samples
"""

def plot_contour(x,y,z,resolution = 50,contour_method='linear'):
    resolution = str(resolution)+'j'
    X,Y = np.mgrid[min(x):max(x):complex(resolution),   min(y):max(y):complex(resolution)]
    points = [[a,b] for a,b in zip(x,y)]
    Z = griddata(points, z, (X, Y), method=contour_method)
    return X,Y,Z

"""
# Linear interpolation
"""
PX,PY,RZ = plot_contour(px,py,rr,resolution = int(rr.shape[0]),contour_method='linear')

ax[1,0].contourf(PX,
                  img_orig.shape[0]-PY, 
                 RZ, 
                 cmap = 'gray', vmin = img_orig.min(), vmax = img_orig.max(),
                 levels = img_orig.max() )

"""
# Bicubic interpolation
"""
BX,BY,BZ = plot_contour(px,py,rr,resolution = int(rr.shape[0]),contour_method='cubic')
ax[1,1].contourf(BX, 
                  img_orig.shape[0]-BY, 
                 BZ, 
                 cmap = 'gray', vmin = img_orig.min(), vmax = img_orig.max(),
                 levels = img_orig.max() )
"""
# show plot
"""
plt.tight_layout()
plt.show()
