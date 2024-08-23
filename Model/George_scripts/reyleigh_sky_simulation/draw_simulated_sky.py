import numpy as np
import matplotlib.pyplot as plt
import matplotlib.colors as mcolors
from PIL import Image
import sys

input_file = sys.argv[1]  
output_image = sys.argv[2]

data = np.loadtxt(input_file, delimiter='\t')
x_coords = data[:, 0].astype(int)
y_coords = data[:, 1].astype(int)
angles = data[:, 2]

# normalize angles from [0, pi] to [0, 1]
angles_normalized = (angles % (np.pi)) / (np.pi)

max_x = np.max(x_coords) + 1
max_y = np.max(y_coords) + 1

# empty image array
image_array = np.zeros((max_y, max_x, 3))

cmap = plt.get_cmap('hsv')
norm = mcolors.Normalize(vmin=0, vmax=1)

# map angles to colors
for x, y, angle_norm in zip(x_coords, y_coords, angles_normalized):
    color = cmap(norm(angle_norm))
    image_array[y, x] = color[:3]  # RGB

img = Image.fromarray((image_array * 255).astype(np.uint8))
img.save(output_image)
