from PIL import Image
import sys

def superimpose_images(background_path, overlay_path, output_path):
    # Open the background and overlay images
    background = Image.open(background_path)
    overlay = Image.open(overlay_path)

    # Calculate the center position
    position = (
        (background.width - overlay.width) // 2,
        (background.height - overlay.height) // 2
    )

    # Convert overlay image to RGBA mode
    overlay = overlay.convert("RGBA")

    # Create a new image as the result
    result = Image.new("RGBA", background.size)

    # Paste the background onto the result
    result.paste(background, (0, 0))

    # Paste the overlay with transparency at the calculated center position
    result.paste(overlay, position, overlay)

    # Save the result
    result.save(output_path, "PNG")

if __name__ == "__main__":
    if len(sys.argv) != 4:
        print("Usage: python script.py background_image overlay_image output_image")
        sys.exit(1)

    background_image_path = sys.argv[1]
    overlay_image_path = sys.argv[2]
    output_image_path = sys.argv[3]
    
    superimpose_images(background_image_path, overlay_image_path, output_image_path)
    
    #print("Images superimposed and saved as", output_image_path)
