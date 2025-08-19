# this script creates a colorbar based on an image. It uses the brightest and darkest colors as the colorbar's edges and interpolates the rest of the colors.

import cv2
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.colors import LinearSegmentedColormap
import sys

# Load your grayscale image
image = cv2.imread(sys.argv[1], cv2.IMREAD_GRAYSCALE)

# Create a colormap based on the grayscale intensity
cmap = plt.cm.gray

# Create a colorbar using Matplotlib
fig, ax = plt.subplots(figsize=(6, 1))
cbar = plt.colorbar(plt.imshow(image, cmap=cmap, vmin=0, vmax=1), cax=ax, orientation='horizontal', aspect=10)

# Display the colorbar
plt.show()

