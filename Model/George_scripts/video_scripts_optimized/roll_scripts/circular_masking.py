# usage: python circular_masking.py <infile_path> <outfile_path>
# author: Georgios Kolyfetis

from PIL import Image, ImageDraw
import sys

def make_circular_mask(im):
    # Create a circular mask with the same size as the input image
    mask = Image.new("L", im.size, 0)
    draw = ImageDraw.Draw(mask)

    # Calculate the center coordinates
    center_x = im.width // 2
    center_y = im.height // 2

    # Calculate the radius as half of the minimum dimension
    radius = min(center_x, center_y)

    # Draw a circular mask within the bounding box
    draw.ellipse((center_x - radius, center_y - radius, center_x + radius, center_y + radius), fill=255)

    return mask

def apply_circular_mask(im, mask):
    # Create a copy of the original image with an alpha channel
    out = im.convert("RGBA")

    # Apply the circular mask to the alpha channel
    out.putalpha(mask)

    return out

if __name__ == "__main__":
    # Open the input image specified in the command-line arguments
    with Image.open(sys.argv[1]) as im:
        # Ensure the image is square (crop if necessary)
        size = min(im.size)
        im = im.crop((0, 0, size, size))

        # Create the circular mask
        circular_mask = make_circular_mask(im)

        # Apply the circular mask to the image
        result_image = apply_circular_mask(im, circular_mask)

        # Save the resulting image with transparency
        result_image.save(sys.argv[2])

