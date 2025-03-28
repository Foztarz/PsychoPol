import os
import sys
import numpy as np
import matplotlib.pyplot as plt
import skimage.io

# Load image data
images_pola = np.load(sys.argv[1], allow_pickle=True)  # .npy file (day)
print(images_pola.shape)
image_pola = images_pola[int(sys.argv[2])]  # choose image of that day
print(image_pola)  # shows time taken
print(image_pola[1])  # exposure time in µs

# Display image
plt.imshow(image_pola[0], cmap="gray")  # Use grayscale colormap
plt.grid(False)
plt.colorbar()
#plt.show()
#print(image_pola[0]) # this is 16-bit


output_filename = sys.argv[3]  # Output image
skimage.io.imsave(output_filename, image_pola[0], check_contrast=False)

print(f"Saved 16-bit TIFF: {output_filename}")
