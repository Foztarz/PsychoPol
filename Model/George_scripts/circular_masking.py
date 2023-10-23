# usage: python circular_masking.py <infile_path> <desired_radius_in_pixels> <outfile_path>
# author: Georgios Kolyfetis

from PIL import Image,ImageDraw,ImageOps
import sys



def make_circular(im, radius=None):
    if radius is None:
        # Use the default radius if not specified
        radius = min(im.width, im.height) / 2

    # Create a blank image with the same size as the input image
    mask = Image.new("L", im.size, 0)
    draw = ImageDraw.Draw(mask)

    # Calculate the center coordinates
    center_x = im.width // 2
    center_y = im.height // 2

    # Calculate the bounding box for the circle based on the specified radius
    box = (center_x - int(radius), center_y - int(radius), center_x + int(radius), center_y + int(radius))

    # Draw a circular mask within the bounding box
    draw.ellipse(box, fill=255)

    # Create an image with the circular mask
    out = Image.new("RGBA", im.size)
    out.paste(im, (0, 0))
    out.putalpha(mask)

    return out




if __name__ == "__main__":
    with Image.open(sys.argv[1]) as im:
        im = im.convert("RGBA")
        # Specify the desired radius
        desired_radius = sys.argv[2]

        # Create the circular mask with the specified radius
        circular_im = make_circular(im, radius=desired_radius)
        circular_im.save(sys.argv[3])
