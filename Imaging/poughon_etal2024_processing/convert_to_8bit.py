import cv2
import numpy as np
import sys
import os

def convert_to_8bit(img: np.ndarray) -> np.ndarray:
    """Converts a 16-bit image to an 8-bit grayscale image."""
    img_8bit = (img.astype(np.float32) * 255 / 65535) ## no rounding
    return img_8bit

# 16-bit TIFF image
image_path = sys.argv[1]
img = cv2.imread(image_path, cv2.IMREAD_UNCHANGED)  # load as is (16-bit)

# convert to 8-bit
img_8bit = convert_to_8bit(img)

base_name = os.path.splitext(os.path.basename(image_path))[0]  
output_path = f"{base_name}_8bit.npy"  

# NumPy array
np.save(output_path, img_8bit)

print(f"8-bit grayscale NumPy array saved to: {output_path}")
