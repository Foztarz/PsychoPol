import os, sys
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation
from tqdm import trange
import skimage.io
import skimage.filters
import time
from scipy.spatial.transform import Rotation as R
from skimage.io import imsave

from tqdm import tqdm
from p_tqdm import p_map

import csv

images_pola=np.load(sys.argv[1],allow_pickle=True) ## .npy file (day)
print(images_pola.shape)
image_pola=images_pola[200] ## choose which image of that day
plt.imshow(image_pola[0])
plt.grid(False)
plt.colorbar()

plt.show()

output_filename = sys.argv[2] ## output image
skimage.io.imsave(output_filename, image_pola[0])

