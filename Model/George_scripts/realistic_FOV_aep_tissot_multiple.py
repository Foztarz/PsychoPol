import sys
import numpy as np
import cv2

def spherical_to_cartesian(radius, azimuth_deg, elevation_deg):
    s = radius * elevation_deg / 90  # distance from image edge
    azimuth_rad = np.radians(azimuth_deg)
    elevation_rad = np.radians(elevation_deg)
    x = int((radius - s) * np.sin(azimuth_rad))
    y = int(radius - (radius - s) * np.cos(azimuth_rad))
    return x, y

def main(image_path, output_path, coordinates_file, minor_axis):
    try:

        # Open the circular image
        img = cv2.imread(image_path)        
        
        # Calculate the center of the projection
        img_height, img_width, _ = img.shape
        center_x = img_width // 2
        center_y = img_height // 2

        # this is for rotating the image if necessary (bicubic interpolation). Note that it rotates counterclockwise for positive angles
        M = cv2.getRotationMatrix2D((center_y,center_x),0,1) # the format is cv2.getRotationMatrix2D(center, angle, scale) 
        img = cv2.warpAffine(img,M,(img_width,img_height),flags=cv2.INTER_CUBIC)

        # Create a blank canvas with a white background
        canvas = np.full_like(img, (255, 255, 255), dtype=np.uint8) # activate this for white background
        #canvas = np.zeros_like(img) # activate this for transparent canvas (shows the image)
        
        # Draw the rotated ellipse on the canvas
        color = (0, 0, 255)  # Color red in BGR format
        
        # Read the coordinates from the file
        with open(coordinates_file, 'r') as file:
            for line in file:
                azimuth_deg, elevation_deg = map(float, line.strip().split('\t'))

                # Calculate the pixel coordinates for the projection
                projection_radius = min(center_x, center_y)
                proj_x, proj_y = spherical_to_cartesian(projection_radius, azimuth_deg, elevation_deg)
                proj_x += center_x  # This is to set 0,0 to the north (top of the image)
                
                # Calculate the axes lengths for the ellipse and distortion based on azimuth
                minor_axis = int(minor_axis)
                if elevation_deg == 90: # this is for the unlikely case of 90deg elevation. Normally a limit has to be calculated.
                    distortion = 1
                else:
                    distortion = float(((np.pi/2) - np.pi * elevation_deg/180) / np.cos(np.pi * elevation_deg/180))
                #print(distortion)
                major_axis = int(distortion * minor_axis)
                #print(major_axis, minor_axis)
                
                # Draw the rotated ellipse on the canvas, filling the ellipse with red color
                thickness = -1  # -1 thickness fills the ellipse, thickness = 2 for transparent ellipses
                angle = azimuth_deg  # Rotation angle in degrees (for the ellipses, not the image)
                cv2.ellipse(canvas, (proj_x, proj_y), (major_axis, minor_axis), angle, 0, 360, color, thickness)

        # Overlay the canvas with the ellipses on the original image
        #result = cv2.addWeighted(img, 1, canvas, 1, 0)
        #result = canvas # activate this for showing only the ommatidia (ellipses) without the image
        # Save the resulting image
        cv2.imwrite(output_path, canvas)
        print(f"Projection image saved to {output_path}")

    except Exception as e:
        print(f"An error occurred: {str(e)}")

if __name__ == "__main__":
    if len(sys.argv) != 5:
        print("Usage: python script.py <input_image> <output_image> <coordinates_file> <minor_axis>")
        sys.exit(1)

    input_image = sys.argv[1]
    output_image = sys.argv[2]
    coordinates_file = sys.argv[3]
    minor_axis = sys.argv[4]

    main(input_image, output_image, coordinates_file, minor_axis)
