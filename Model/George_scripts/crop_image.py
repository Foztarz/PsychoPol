# usage: python crop_image.py <input_image>
# author: Georgios Kolyfetis

from PIL import Image,ImageOps
import sys

img = Image.open(sys.argv[1])

border = (383, 183, 382, 182) # left, top, right, bottom
img2 = ImageOps.crop(img, border)
img2.save(sys.argv[2])
