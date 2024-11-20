## usage: python demosaicing.py <input_image>

import numpy as np
from matplotlib import pyplot as plt
from matplotlib.pylab import cm
import cv2
import sys
import os
import warnings
from PIL import Image
from dataclasses import dataclass
from typing import List

@dataclass
class ColorConversionCode:
    is_color: bool
    suffix: str  # suffix for OpenCV's ColorConversionCodes (i.e. "", "_VNG", "_EA")

# Bilinear interpolation
COLOR_PolarRGB = ColorConversionCode(is_color=True, suffix="")
COLOR_PolarMono = ColorConversionCode(is_color=False, suffix="")
# Variable Number of Gradients
COLOR_PolarRGB_VNG = ColorConversionCode(is_color=True, suffix="_VNG")
COLOR_PolarMono_VNG = ColorConversionCode(is_color=False, suffix="_VNG")
# Edge-Aware
COLOR_PolarRGB_EA = ColorConversionCode(is_color=True, suffix="_EA")
COLOR_PolarMono_EA = ColorConversionCode(is_color=False, suffix="_EA")

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

def demosaicing(img_raw: np.ndarray, code: ColorConversionCode = COLOR_PolarRGB) -> List[np.ndarray]:
    """Polarization demosaicing

    Parameters
    ----------
    img_raw : np.ndarray
        Polarization image taken with polarizatin sensor (e.g. IMX250MZR or IMX250MYR sensor). The shape is (height, width).
    code : ColorConversionCode, optional
        Color space conversion code, by default `COLOR_PolarMono`

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
    
def extract_third_channel(img: np.ndarray) -> np.ndarray:
    _, _, channel_3 = cv2.split(img)  # extract the blue channel if RGB
    return channel_3

hdr_image_path = sys.argv[1]

if not os.path.isfile(hdr_image_path):
    print(f"Error: The file {hdr_image_path} does not exist.")
    sys.exit(1)

# Load the HDR image
img_HDR = cv2.imread(hdr_image_path, cv2.IMREAD_ANYDEPTH)

if img_HDR is None:
    print(f"Error: Could not read the HDR image from {hdr_image_path}. Ensure it is a valid HDR image.")
    sys.exit(1)

# Normalize the HDR image for display
img_HDR_normalized = img_HDR.astype(np.float64) / img_HDR.max()

# HDR image demosaicing
img_000, img_045, img_090, img_135 = demosaicing(img_HDR)

img_000_gray = extract_third_channel(img_000)
img_045_gray = extract_third_channel(img_045)
img_090_gray = extract_third_channel(img_090)
img_135_gray = extract_third_channel(img_135)
cv2.imshow("img_000_blue", img_000_gray)
cv2.imshow("img_045_blue", img_045_gray)
cv2.imshow("img_090_blue", img_090_gray)
cv2.imshow("img_135_blue", img_135_gray)
##cv2.imshow("img_000.png", img_000.astype(np.float64) / img_000.max())
##cv2.imshow("img_045.png", img_045.astype(np.float64) / img_045.max())
##cv2.imshow("img_090.png", img_090.astype(np.float64) / img_090.max())
##cv2.imshow("img_135.png", img_135.astype(np.float64) / img_135.max())

# Display the final HDR image using cv2.imshow()
cv2.imshow('Final HDR Image', img_HDR_normalized)

# Wait for a key press and then close the window
cv2.waitKey(0)
cv2.destroyAllWindows()

