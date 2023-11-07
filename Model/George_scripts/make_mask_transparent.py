from PIL import Image
import sys

def convert_red_to_transparent(input_image_path, output_image_path):
    # Open the input image using Pillow
    image = Image.open(input_image_path)

    # Convert the image to RGBA mode (to support transparency)
    image = image.convert("RGBA")

    # Get the pixel data as a list of (R, G, B, A) tuples
    pixel_data = list(image.getdata())

    # Define the red color threshold
    red_threshold = (255, 0, 0, 255)  # RGBA values for red (#ff0000)

    # Create a new list of pixel data with red pixels converted to transparent
    new_pixel_data = [(r, g, b, 0) if (r, g, b, a) == red_threshold else (r, g, b, a) for (r, g, b, a) in pixel_data]

    # Update the image with the new pixel data
    image.putdata(new_pixel_data)

    # Save the modified image
    image.save(output_image_path)


if __name__ == "__main__":
    input_image_path = sys.argv[1]
    output_image_path = sys.argv[2]
    convert_red_to_transparent(input_image_path, output_image_path)
