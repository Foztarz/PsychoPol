import cv2
import numpy as np
import sys

# Load the image and mask
image_path = sys.argv[1]
mask_path = sys.argv[2]

img = cv2.imread(image_path)
mask = cv2.imread(mask_path, cv2.IMREAD_UNCHANGED)  # Read as a 4-channel image

# Get image dimensions
img_height, img_width = img.shape[:2]
center_x, center_y = img_width // 2, img_height // 2

# Iterate over rotation angles
for angle in range(0, 360, 5):
    # Rotate the image
    M = cv2.getRotationMatrix2D((center_x, center_y), angle, 1)
    rotated_img = cv2.warpAffine(img, M, (img_width, img_height), flags=cv2.INTER_CUBIC)

    # Calculate the region to overlay the mask on the rotated image
    start_y = max(center_y - mask.shape[0] // 2, 0)
    end_y = min(center_y + mask.shape[0] // 2, img_height)
    start_x = max(center_x - mask.shape[1] // 2, 0)
    end_x = min(center_x + mask.shape[1] // 2, img_width)

    # Resize mask to match the rotated image size
    resized_mask = cv2.resize(mask, (end_x - start_x, end_y - start_y))

    # Create a mask for blending based on the alpha channel
    mask_alpha = resized_mask[:, :, 3] / 255.0
    mask_alpha = np.stack([mask_alpha] * 3, axis=-1)

    # Blend the rotated image and the resized mask using the alpha channel
    result = rotated_img.copy()
    result[start_y:end_y, start_x:end_x] = (1 - mask_alpha) * result[start_y:end_y, start_x:end_x] + mask_alpha * resized_mask[:, :, :3]


    # Save the result
    output_path = f'background_with2eyes_{angle}.png'
    cv2.imwrite(output_path, result)

print("Processing complete.")

