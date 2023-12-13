# this script makes FOVs/ellipses with color that matches the PRC (photoreceptor contrast) values (bwr colormap) for the second eye

import sys
import numpy as np
import cv2
import matplotlib
import matplotlib.cm as cm
import colorsys


def spherical_to_cartesian(radius, azimuth_deg, elevation_deg):
    s = radius * elevation_deg / 90  # distance from image edge
    azimuth_rad = np.radians(azimuth_deg)
    elevation_rad = np.radians(elevation_deg)
    x = int((radius - s) * np.sin(azimuth_rad))
    y = int(radius - (radius - s) * np.cos(azimuth_rad))
    return x, y

def main(image_path, output_path, azimuth_list, elevation_list, PRC_list, minor_axis):
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
        
        for azimuth_deg, elevation_deg, PRC_value in zip(azimuth_list, elevation_list, PRC_list):
       
            # Calculate the pixel coordinates for the projection
            projection_radius = min(center_x, center_y)
            proj_x, proj_y = spherical_to_cartesian(projection_radius, azimuth_deg, elevation_deg)
            proj_x += center_x  # This is to set 0,0 to the north (top of the image)

            # this is for the second eye (mirrored)
            if proj_x >= center_x:
                proj_x2 = proj_x - 2*(proj_x-center_x)
            elif proj_x < center_x:
                proj_x2 = proj_x + 2*(center_x-proj_x)
            
            # Calculate the axes lengths for the ellipse and distortion based on azimuth
            minor_axis = int(minor_axis)
            if elevation_deg == 90: # this is for the unlikely case of 90deg elevation. Normally a limit has to be calculated.
                distortion = 1
            else:
                distortion = float(((np.pi/2) - np.pi * elevation_deg/180) / np.cos(np.pi * elevation_deg/180))
            #print(distortion)
            major_axis = int(distortion * minor_axis)
            #print(major_axis, minor_axis)
            
            # Activate these lines for interpolation for AoP
            #print(color_value)
            colormap = matplotlib.colormaps["bwr"]
            #color_value = color_value/np.pi # scale from 0 to 1
            rgba_color = colormap(PRC_value)
            rgba_color = tuple(int(i * 255) for i in rgba_color) # make to rgba
            
            # Draw the rotated ellipse on the canvas, filling the ellipse with red color
            thickness = -1  # -1 thickness fills the ellipse, thickness = 2 for transparent ellipses
            angle = azimuth_deg  # Rotation angle in degrees (for the ellipses, not the image)
            cv2.ellipse(canvas, (proj_x2, proj_y), (major_axis, minor_axis), angle, 0, 360, rgba_color, thickness)

        canvas = cv2.cvtColor(canvas, cv2.COLOR_RGB2BGR)
        # Save the resulting image
        cv2.imwrite(output_path, canvas)
        #print(f"Projection image saved to {output_path}")

    except Exception as e:
        print(f"An error occurred: {str(e)}")

if __name__ == "__main__":
    if len(sys.argv) != 7:
        print("Usage: python script.py <input_image> <output_image> <azimuth_list> <elevation_list> <PRC_list> <minor_axis>")
        sys.exit(1)

    input_image = sys.argv[1]
    output_image = sys.argv[2]
    azimuth_list = [float(x.strip('[]')) for x in sys.argv[3].split(',')]
    elevation_list = [float(x.strip('[]')) for x in sys.argv[4].split(',')]
    PRC_list = [float(x.strip('[]')) for x in sys.argv[5].split(',')]
    minor_axis = sys.argv[6]
    
    main(input_image, output_image, azimuth_list, elevation_list, PRC_list, minor_axis)
