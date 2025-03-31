# this script makes lines for AoLP instead of ellipses for the second eye, the width of the lines is relative to the DoLP

import sys
import numpy as np
import cv2
import matplotlib
import matplotlib.cm as cm
import colorsys
import math

def spherical_to_cartesian(radius, azimuth_deg, elevation_deg):
    s = radius * elevation_deg / 90  # distance from image edge
    azimuth_rad = np.radians(azimuth_deg)
    elevation_rad = np.radians(elevation_deg)
    x = int((radius - s) * np.sin(azimuth_rad))
    y = int(radius - (radius - s) * np.cos(azimuth_rad))
    return x, y

def main(image_path, output_path, azimuth_list, elevation_list, aolp_list, dolp_list):
    try:
        # Open the circular image
        img = np.load(image_path)
        # Calculate the center of the projection
        img_height, img_width = img.shape
        center_x = img_width // 2
        center_y = img_height // 2

        # this is for rotating the image if necessary (bicubic interpolation). Note that it rotates counterclockwise for positive angles
        M = cv2.getRotationMatrix2D((center_y, center_x), 0, 1)  # the format is cv2.getRotationMatrix2D(center, angle, scale)
        img = cv2.warpAffine(img, M, (img_width, img_height), flags=cv2.INTER_CUBIC)

        # Create a blank canvas with a white background
        canvas = np.full((img.shape[0], img.shape[1], 3), (255, 255, 255), dtype=np.uint8) # add a 3rd channel for image visualization, does not affect calculations
        #canvas = np.zeros_like(img) # activate this for transparent canvas (shows the image)
        for azimuth_deg, elevation_deg, aolp_value, dolp_value in zip(azimuth_list, elevation_list, aolp_list, dolp_list):
            # Calculate the pixel coordinates for the projection
            projection_radius = min(center_x, center_y)
            proj_x, proj_y = spherical_to_cartesian(projection_radius, azimuth_deg, elevation_deg)
            proj_x += center_x  # This is to set 0,0 to the north (top of the image)
            
            # this is for the second eye (mirrored)
            if proj_x >= center_x:
                proj_x2 = proj_x - 2*(proj_x-center_x)
            elif proj_x < center_x:
                proj_x2 = proj_x + 2*(center_x-proj_x)
                
            # Calculate line endpoints
            line_length = 15  # line length
            angle_rad = -aolp_value # negative because of image coordinates
            x1 = int(proj_x2 - line_length * np.cos(angle_rad))
            y1 = int(proj_y - line_length * np.sin(angle_rad))
            x2 = int(proj_x2 + line_length * np.cos(angle_rad))
            y2 = int(proj_y + line_length * np.sin(angle_rad))

            # draw lines or circles (DoP close to 0)
            if dolp_value < 0.01: # this is for the unlikely case of dolp_value < 0.01, draws dot
                cv2.circle(canvas, (proj_x2, proj_y), radius=3, color=(0, 0, 0), thickness=-1)
            # Draw the line on the canvas
            else:
                line_width = int(math.ceil(dolp_value * 15))
                if line_width == 0: # this is when dolp > 0.1 but line width still 0
                    line_width = 1 # minimum line width
                    cv2.line(canvas, (x1, y1), (x2, y2), color=(0, 0, 0), thickness=line_width)
                else:
                    cv2.line(canvas, (x1, y1), (x2, y2), color=(0, 0, 0), thickness=line_width)

        canvas = cv2.cvtColor(canvas, cv2.COLOR_RGB2BGR)
        # Save the resulting image
        cv2.imwrite(output_path, canvas)
        #print(f"Projection image saved to {output_path}")

    except Exception as e:
        print(f"An error occurred: {str(e)}")

if __name__ == "__main__":
    if len(sys.argv) != 7:
        print("Usage: python script.py <input_image> <output_image> <azimuth_list> <elevation_list> <aolp_list> <dolp_list>")
        sys.exit(1)

    input_image = sys.argv[1]
    output_image = sys.argv[2]
    azimuth_list = [float(x.strip('[]')) for x in sys.argv[3].split(',')]
    elevation_list = [float(x.strip('[]')) for x in sys.argv[4].split(',')]
    aolp_list = [float(x.strip('[]')) for x in sys.argv[5].split(',')]
    dolp_list = [float(x.strip('[]')) for x in sys.argv[6].split(',')]

    main(input_image, output_image, azimuth_list, elevation_list, aolp_list, dolp_list)
