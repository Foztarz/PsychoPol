# usage: python crop_image.py <input_image>
# author: Georgios Kolyfetis

from PIL import Image,ImageOps
import sys

img = Image.open(sys.argv[1])

border = (105, 105, 105, 105) # left, top, right, bottom
img2 = ImageOps.crop(img, border)
img2.show()
