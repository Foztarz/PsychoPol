from PIL import Image
import sys

def convert_transparent_to_white_and_non_white_to_transparent(input_path, output_path):
    # Open the image
    img = Image.open(input_path)

    # Convert to RGBA if the image doesn't have an alpha channel
    img = img.convert('RGBA')

    # Get the pixel data as a list of (R, G, B, A) tuples
    pixel_data = list(img.getdata())

    # Create a new list of pixel data with transparent pixels set to white
    # and non-white pixels set to transparent
    new_pixel_data = [
        (255, 255, 255, 255) if a == 0 else (0, 0, 0, 0) if (r, g, b) != (255, 255, 255) else (r, g, b, a)
        for (r, g, b, a) in pixel_data
    ]

    # Create a new image with the same size and paste the new pixel data
    new_img = Image.new('RGBA', img.size)
    new_img.putdata(new_pixel_data)

    # Save the result
    new_img.save(output_path)

if __name__ == "__main__":
    if len(sys.argv) != 3:
        print("Usage: python script_name.py input_image.png output_image.png")
        sys.exit(1)

    input_image_path = sys.argv[1]
    output_image_path = sys.argv[2]

    convert_transparent_to_white_and_non_white_to_transparent(input_image_path, output_image_path)
