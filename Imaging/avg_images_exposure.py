import os
import numpy as np
from PIL import Image
import sys


# normalize for greyscale
def normalize_image(image_array):
    min_val = np.min(image_array)
    max_val = np.max(image_array)
    norm_image = 255 * (image_array - min_val) / (max_val - min_val)
    return norm_image.astype(np.uint8)

def process_images(image_folder, exposure):

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

    # divide the sum by the given exposure
    avg_image = sum_image / exposure

    # normalize
    norm_image = normalize_image(avg_image)

    # convert the array back to an image
    result_image = Image.fromarray(norm_image)

    return result_image


if __name__ == "__main__":
    # sys.argv[1] should be the folder path and sys.argv[2] should be the exposure
    image_folder = sys.argv[1]
    exposure = float(sys.argv[2])

    result_image = process_images(image_folder, exposure)

    if result_image:
        result_image.save(sys.argv[3])
