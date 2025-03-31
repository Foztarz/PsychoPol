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
from dataclasses import dataclass
from typing import List

@dataclass
class ColorConversionCode:
    is_color: bool
    suffix: str  # suffix for OpenCV's ColorConversionCodes (i.e. "", "_VNG", "_EA")

    
"""
## Input params
"""
fileformat = '.npy'
expos_type = 'name'#exposure is '--######us'
edge_lim = 1.56#% bottom and top 1.56% replaced
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
npy_files = np.take(imdir, np.where( [ff.endswith(fileformat) for ff in imdir] ) )
#WIP, this indexing was a nightmare!
# Bilinear interpolation
COLOR_PolarRGB = ColorConversionCode(is_color=True, suffix="")
COLOR_PolarMono = ColorConversionCode(is_color=False, suffix="")
# Variable Number of Gradients
COLOR_PolarRGB_VNG = ColorConversionCode(is_color=True, suffix="_VNG")
COLOR_PolarMono_VNG = ColorConversionCode(is_color=False, suffix="_VNG")
# Edge-Aware
COLOR_PolarRGB_EA = ColorConversionCode(is_color=True, suffix="_EA")
COLOR_PolarMono_EA = ColorConversionCode(is_color=False, suffix="_EA")

"""
## Find exposure values
"""
def Extract_exposures(label):
    extrExp = {
        'name': [np.float64(ff.split('--')[1].split('us')[0])/1000 for ff in npy_files[0]],
        'exif': warnings.warn('not implemented')
        }
    return(extrExp.get(label, 'Exposure type not implemented'))
exposures = Extract_exposures(expos_type)

def demosaicing(img_raw: np.ndarray, code: ColorConversionCode = COLOR_PolarMono) -> List[np.ndarray]:
    """Polarization demosaicing

    Parameters
    ----------
    img_raw : np.ndarray
        Polarization image taken with polarizatin sensor (e.g. IMX250MZR or IMX250MYR sensor). The shape is (height, width).
    code : ColorConversionCode, optional
        Color space conversion code, by default `pa.COLOR_PolarMono`

    Returns
    -------
    img_demosaiced_list : List[np.ndarray]
        List of demosaiced images. The shape of each image is (height, width) or (height, width, 3).
    """
    if not isinstance(code, ColorConversionCode):
        raise TypeError(f"The type of 'code' must be 'ColorConversionCode', not {type(code)}")

    dtype = img_raw.dtype

    if np.issubdtype(dtype, np.floating):
        # If the dtype is floting type, the image is converted into `uint16` to apply demosaicing process.
        # It may cause inaccurate result.
        scale = 65535.0 / np.max(img_raw)
        img_raw_u16 = np.clip(img_raw * scale, 0, 65535).astype(np.uint16)
        img_demosaiced_u16 = demosaicing(img_raw_u16, code)
        img_demosaiced = (img_demosaiced_u16 / scale).astype(img_raw.dtype)
        return img_demosaiced

    if dtype not in [np.uint8, np.uint16]:
        raise TypeError(f"The dtype of input image must be `np.uint8` or `np.uint16`, not `{dtype}`")

    if img_raw.ndim != 2:
        raise ValueError(f"The dimension of the input image must be 2, not {img_raw.ndim} {img_raw.shape}")

    if code.is_color:
        return __demosaicing_color(img_raw, code.suffix)
    else:
        return __demosaicing_mono(img_raw, code.suffix)

def __demosaicing_mono(img_mpfa: np.ndarray, suffix: str = "") -> List[np.ndarray]:
    """Polarization demosaicing for np.uint8 or np.uint16 type"""
    code_bg = getattr(cv2, f"COLOR_BayerBG2BGR{suffix}")
    code_gr = getattr(cv2, f"COLOR_BayerGR2BGR{suffix}")
    img_debayer_bg = cv2.cvtColor(img_mpfa, code_bg)
    img_debayer_gr = cv2.cvtColor(img_mpfa, code_gr)
    img_000, _, img_090 = cv2.split(img_debayer_bg)
    img_045, _, img_135 = cv2.split(img_debayer_gr)
    return [img_000, img_045, img_090, img_135]


def __demosaicing_color(img_cpfa: np.ndarray, suffix: str = "") -> List[np.ndarray]:
    """Color-Polarization demosaicing for np.uint8 or np.uint16 type"""
    height, width = img_cpfa.shape[:2]

    # 1. Color demosaicing process
    img_mpfa_bgr = np.empty((height, width, 3), dtype=img_cpfa.dtype)
    code = getattr(cv2, f"COLOR_BayerBG2BGR{suffix}")
    for j in range(2):
        for i in range(2):
            # (i, j)
            # (0, 0) is 90,  (0, 1) is 45
            # (1, 0) is 135, (1, 1) is 0

            # Down sampling ↓2
            img_bayer_ij = img_cpfa[j::2, i::2]
            # Color demosaicking
            img_bgr_ij = cv2.cvtColor(img_bayer_ij, code)
            # Up samping ↑2
            img_mpfa_bgr[j::2, i::2] = img_bgr_ij

    # 2. Polarization demosaicing process
    img_bgr_000 = np.empty((height, width, 3), dtype=img_mpfa_bgr.dtype)
    img_bgr_045 = np.empty((height, width, 3), dtype=img_mpfa_bgr.dtype)
    img_bgr_090 = np.empty((height, width, 3), dtype=img_mpfa_bgr.dtype)
    img_bgr_135 = np.empty((height, width, 3), dtype=img_mpfa_bgr.dtype)
    for i, img_mpfa in enumerate(cv2.split(img_mpfa_bgr)):
        img_000, img_045, img_090, img_135 = __demosaicing_mono(img_mpfa, suffix)
        img_bgr_000[..., i] = img_000
        img_bgr_045[..., i] = img_045
        img_bgr_090[..., i] = img_090
        img_bgr_135[..., i] = img_135

    return [img_bgr_000, img_bgr_045, img_bgr_090, img_bgr_135]

"""
## Read in files as list
"""

npy_files = npy_files[0].tolist()  # Convert ndarray to a list
imgs_raw = [np.load(imfl) for imfl in npy_files]

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
## Convert to units of intensity/second
"""
def transform_intensity(image):
    transformed = image.astype(np.float64)  # float for calculations

    # transform pixels in range [0, 249] (linear)
    mask1 = (image >= 0) & (image <= 249)
    transformed[mask1] = np.where(transformed[mask1] == 0, 10.0912, ((transformed[mask1] + 0.3129)/ 0.0156))
    # we replace 0s with 0, 10.0912 because this is the average intensity that corresponds to px value 0f 0 (after solving np.round(0.0156x - 0.3129) =< 0, for x>0 and scaling by (2^16-1)/(2^8-1))
    # y = 0.0156x - 0.3129 ## original function
    
    # transform pixels in range [249, 255] (sigmoid)
    mask2 = (image >= 250) & (image <= 255)
    transformed[mask2] = np.where(transformed[mask2] == 255, 20233.211948780852, (1.12663079e+04 - (1 / 8.24e-04) * np.log((255 / (transformed[mask2] - 0.155638819)) - 1)))
    # the intensity prediction for px value equal to 254.9981 (equivalent of 254.5 of 8-bit) is 20233.211948780852, so we use that as maximum intensity
    # y = 255 / (1 + np.exp(-8.24e-04 * (x - 1.12663079e+04))) + 0.155638819   ## original function
    return transformed

# process all images
imgs_transformed = [transform_intensity(img) for img in imgs_raw]

imgs_bytes_s = imgs_transformed
for ii in range(len(imgs_transformed)):
    imgs_bytes_s[ii] = np.float64(imgs_transformed[ii]) / exposures[ii]
    
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
HDR_histogram = np.histogram(img_HDR_val, bins = np.uint32(1e3), 
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
img_mid_histogram = np.histogram(img_mid_val, bins = np.uint32(1e3), 
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

# HDR image demosaicing
#img_000, img_045, img_090, img_135 = demosaicing(img_HDR)
img_000, img_045, img_090, img_135 = demosaicing(img_HDR, COLOR_PolarRGB) 

##cv2.imshow("img_000.png", img_000[:,:,0].astype(np.float64)/img_000.max())
##cv2.imshow("img_045.png", img_045[:,:,0].astype(np.float64)/img_045.max())
##cv2.imshow("img_090.png", img_090[:,:,0].astype(np.float64)/img_090.max())
##cv2.imshow("img_135.png", img_135[:,:,0].astype(np.float64)/img_135.max())

##img45_hist = img_045[:,:,0].ravel()
##fig = plt.figure()
##ax = fig.add_subplot(111)
##img45_histogram = np.histogram(img45_hist, bins = np.uint32(1e3), 
##                             range = [0, 
##                                      np.nanmax(img45_hist)])
##brs = ax.bar(x =  img45_histogram[1][1:], 
##             height = img45_histogram[0],
##             ec="k", align="edge")
##fig.savefig( os.path.dirname(imfile)+ '/img45_red_histogram.pdf')
##
##img45_hist = img_045[:,:,1].ravel()
##fig = plt.figure()
##ax = fig.add_subplot(111)
##img45_histogram = np.histogram(img45_hist, bins = np.uint32(1e3), 
##                             range = [0, 
##                                      np.nanmax(img45_hist)])
##brs = ax.bar(x =  img45_histogram[1][1:], 
##             height = img45_histogram[0],
##             ec="k", align="edge")
##fig.savefig( os.path.dirname(imfile)+ '/img45_green_histogram.pdf')
##
##img45_hist = img_045[:,:,2].ravel()
##fig = plt.figure()
##ax = fig.add_subplot(111)
##img45_histogram = np.histogram(img45_hist, bins = np.uint32(1e3), 
##                             range = [0, 
##                                      np.nanmax(img45_hist)])
##brs = ax.bar(x =  img45_histogram[1][1:], 
##             height = img45_histogram[0],
##             ec="k", align="edge")
##fig.savefig( os.path.dirname(imfile)+ '/img45_blue_histogram.pdf')

# Display the final HDR image using cv2.imshow()
#cv2.imshow('Final HDR Image', img_HDR.astype(np.float64)/img_HDR.max()) # save this image when it prompts, the cv2.imwrite() doesn't work properly.
np.save('Final_HDR.npy', img_HDR.astype(np.float64)/img_HDR.max())

np.save("img_000.npy", img_000[:,:,2].astype(np.float64)/img_000.max()) ## img_000[:,:,2] is the blue channel
np.save("img_045.npy", img_045[:,:,2].astype(np.float64)/img_045.max())
np.save("img_090.npy", img_090[:,:,2].astype(np.float64)/img_090.max())
np.save("img_135.npy", img_135[:,:,2].astype(np.float64)/img_135.max())

# Wait for a key press and then close the window
cv2.waitKey(0)
cv2.destroyAllWindows()
#plt.show()

if lens_type == 'fisheye' :
    lens_radius = np.float64(img_HDR.shape[1])/2 * (1 - 80/256)
    msk = np.empty(img_HDR.shape[:2], np.float64)
    msk[:] = np.nan
    ctr = (np.float64(img_HDR.shape[:2])+0)/2
    #these loops are VERY slow
    # im_coords = [[row, col] for row in range(0, img_DoLP.shape[0] - 1) for col in range(img_DoLP.shape[1] - 1)]
    # rowcol_distance2= [(np.square(i[0]), np.square(i[1]))  for i in (np.float64(im_coords)-ctr).tolist()]
    # ctr_distance = [(np.sqrt(i[0] + i[1]))  for i in rowcol_distance2]
    # lens_coords = [im_coords[i] for i in lens_ind]
    #instead, construct the coordinates using an array function
    im_coords = np.ones( np.append(2, img_HDR.shape), dtype = np.int16)
    im_coords[0] = im_coords[0] * np.array([range(img_HDR.shape[0])]).T
    im_coords[1] = im_coords[1] * np.array([range(img_HDR.shape[1])])
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
    msk = np.ones(img_HDR.shape[:2], np.float64)

    
def  Scale_sigmoid(x, inflex = 0., width = 2., rang = 0.8):
    bx = (2*np.log((1/((1-rang)/2))-1)*(x-inflex))/width
    yy = 1/(1+np.exp(-(bx)))
    return(yy)

# Calculate the Stokes vector per-pixel
radians = np.array([0, np.pi/4, np.pi/2, np.pi*3/4])
img_stokes = pa.calcStokes(demosaiced_img, radians)

img_intensity = pa.cvtStokesToIntensity(img_stokes)
img_intensity_msk = img_intensity.astype(np.float64) * msk
nonzero = np.log10( img_intensity_msk[np.nonzero(img_intensity_msk)].ravel() )
img_displ_int = Scale_sigmoid( np.log10( img_intensity_msk ), 
                              inflex= np.nanmedian(nonzero),
                              width = np.diff(np.nanquantile(nonzero, [(1-max_val)/2, 1-(1-max_val)/2])),
                              rang = max_val)
plt.imsave("Final_HDR_forviz.png", img_displ_int, cmap='gray', vmin=0, vmax=1)

