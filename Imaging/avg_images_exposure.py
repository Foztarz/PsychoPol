import os
import numpy as np
from PIL import Image
import sys
import cv2
import tifffile

def process_images(image_folder):

    image_files = [f for f in os.listdir(image_folder) if f.endswith(('png', 'jpg', 'jpeg', 'bmp', 'tiff'))]
    image_files.sort()

    if not image_files:
        print("No images found in the folder.")
        return None

    # images as numpy arrays
    first_image_path = os.path.join(image_folder, image_files[0])
    sum_image = np.array(Image.open(first_image_path).convert('L'), dtype=np.float64)


    for image_file in image_files[1:]:
        image_path = os.path.join(image_folder, image_file)
        img = np.array(Image.open(image_path).convert('L'), dtype=np.float64)
        sum_image += img # add pixel values

    avg_image = sum_image

    return avg_image


if __name__ == "__main__":
    image_folder = sys.argv[1]

    result_image = process_images(image_folder)
    tifffile.imwrite(sys.argv[2], result_image)
    loaded_image = tifffile.imread(sys.argv[2])

    print("Data type:", loaded_image.dtype)
