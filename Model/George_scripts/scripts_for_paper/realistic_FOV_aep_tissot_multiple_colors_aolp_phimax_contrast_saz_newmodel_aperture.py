# this script makes FOVs/ellipses with color that matches the PRC (photoreceptor contrast) values (bwr colormap) for the first eye

import sys
import numpy as np
import cv2
import matplotlib
import matplotlib.cm as cm
import colorsys
import math

def spherical_to_cartesian(radius, azimuth_deg, elevation_deg):
    s = radius * elevation_deg / 90  # distance from the circle's circumference
    azimuth_rad = np.radians(azimuth_deg)
    elevation_rad = np.radians(elevation_deg)
    x = int((radius - s) * np.sin(azimuth_rad))
    y = int(radius - (radius - s) * np.cos(azimuth_rad))
    return x, y

def main(image_path, output_path, azimuth_list, elevation_list, PRC_list, minor_axis, rotation_angle, PRC_colors_list):
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
        
        # Initialize the total solar azimuth vector components (for all ommatidia of this eye)
        total_vector_x = 0
        total_vector_y = 0
        projection_radius = min(center_x, center_y)
                
        for azimuth_deg, elevation_deg, PRC_value, PRC_color in zip(azimuth_list, elevation_list, PRC_list, PRC_colors_list):
            # Calculate the pixel coordinates for the projection
            
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
            

            # Activate these lines for interpolation for AoP
            #print(color_value)
            colormap = matplotlib.colormaps["bwr"]
            #color_value = color_value/np.pi # scale from 0 to 1
            rgba_color = colormap(PRC_color) # using scaled PRC values for colors
            rgba_color = tuple(int(i * 255) for i in rgba_color) # make to rgba

            
            if PRC_value >= 0:
                
                normalized_azimuth_deg = (azimuth_deg % 360 + 360) % 360

                if 0 < normalized_azimuth_deg < 45 or 135 < normalized_azimuth_deg < 225 or 315 < normalized_azimuth_deg < 360: 
                    vector_x = abs(PRC_value) * np.sin(np.radians(180-normalized_azimuth_deg)) # here is the adjustment for the new model, parallel / absolutes because we need positive 
                    vector_y = abs(PRC_value) * np.cos(np.radians(180-normalized_azimuth_deg))
                elif 45 < normalized_azimuth_deg < 135 or 225 < normalized_azimuth_deg < 315:
                    vector_x = abs(PRC_value) * np.sin(np.radians(-normalized_azimuth_deg)) 
                    vector_y = abs(PRC_value) * np.cos(np.radians(-normalized_azimuth_deg))

            if PRC_value < 0:
                
                normalized_azimuth_deg = (azimuth_deg % 360 + 360) % 360

                if 0 < normalized_azimuth_deg < 45 or 180 < normalized_azimuth_deg < 225: # here is the adjustment for the new model, perpendicular 
                    vector_x = abs(PRC_value) * np.sin(np.radians(270-normalized_azimuth_deg))
                    vector_y = abs(PRC_value) * np.cos(np.radians(270-normalized_azimuth_deg))
                elif 45 < normalized_azimuth_deg < 90 or 225 < normalized_azimuth_deg < 270:
                    vector_x = abs(PRC_value) * np.sin(np.radians(-normalized_azimuth_deg-90))
                    vector_y = abs(PRC_value) * np.cos(np.radians(-normalized_azimuth_deg-90))
                elif 90 < normalized_azimuth_deg < 135 or 270 < normalized_azimuth_deg < 315:
                    vector_x = abs(PRC_value) * np.sin(np.radians(-normalized_azimuth_deg+90))
                    vector_y = abs(PRC_value) * np.cos(np.radians(-normalized_azimuth_deg+90))
                elif 135 < normalized_azimuth_deg < 180 or 315 < normalized_azimuth_deg < 360:
                    vector_x = abs(PRC_value) * np.sin(np.radians(90-normalized_azimuth_deg))
                    vector_y = abs(PRC_value) * np.cos(np.radians(90-normalized_azimuth_deg))
    
            # Add the vector components to the total
            total_vector_x += vector_x
            total_vector_y += vector_y
            
            # Draw the rotated ellipse on the canvas, filling the ellipse with red color
            thickness = -1  # -1 thickness fills the ellipse, thickness = 2 for transparent ellipses
            angle = azimuth_deg  # Rotation angle in degrees (for the ellipses, not the image)
            cv2.ellipse(canvas, (proj_x, proj_y), (major_axis, minor_axis), angle, 0, 360, rgba_color, thickness)

        # Calculate the end point of the total vector (normalized to the image size)
        total_vector_angle = float(math.atan2(float(total_vector_x), float(total_vector_y))) # atan2(x, y) because we're using sky coordinates (x axis is vertical) and not mathematical/geometrical (x axis is horizontal)
        end_x = int(center_x + center_x * np.sin(total_vector_angle))
        end_y = int(center_x - center_x * np.cos(total_vector_angle)) # minus for the y coordinate because the center of the image is not 0,0
        print(total_vector_x, total_vector_y) # sending the components to the second eye
        # Draw the line on the canvas
        cv2.line(canvas, (center_x, center_y), (end_x, end_y), color=(0, 0, 255), thickness=2)
        
        # Draw the aperture on the canvas, change accordingly if needed
        azimuth_deg_aperture = 0 - int(rotation_angle)
        elevation_deg_aperture = 60
        
        if elevation_deg_aperture == 90: # this is for the unlikely case of 90deg elevation. Normally a limit has to be calculated.
            distortion_aperture = 1
        else:
            distortion_aperture = float(((np.pi/2) - np.pi * elevation_deg_aperture/180) / np.cos(np.pi * elevation_deg_aperture/180)) # formula for distortion calculation in azimuthal equidistant projections
        
        minor_axis_aperture = 50 # ~10 degrees diameter
        major_axis_aperture = int(distortion_aperture * minor_axis_aperture)
        proj_x_aperture, proj_y_aperture = spherical_to_cartesian(projection_radius, azimuth_deg_aperture, elevation_deg_aperture)
        proj_x_aperture += center_x

        cv2.ellipse(canvas, 
                    (proj_x_aperture, proj_y_aperture), 
                    (major_axis_aperture, minor_axis_aperture), 
                    azimuth_deg_aperture,  
                    0, 360, 
                    255,  # White ellipse (1s)
                    0)  # Filled ellipse
                    
        canvas = cv2.cvtColor(canvas, cv2.COLOR_RGB2BGR)
        # Save the resulting image
        cv2.imwrite(output_path, canvas)
        #print(f"Projection image saved to {output_path}")

    except Exception as e:
        print(f"An error occurred: {str(e)}")

if __name__ == "__main__":
    if len(sys.argv) != 9:
        print("Usage: python script.py <input_image> <output_image> <azimuth_list> <elevation_list> <PRC_list> <minor_axis> <rotation_angle> <PRC_colors_list>")
        sys.exit(1)

    input_image = sys.argv[1]
    output_image = sys.argv[2]
    azimuth_list = [float(x.strip('[]')) for x in sys.argv[3].split(',')]
    elevation_list = [float(x.strip('[]')) for x in sys.argv[4].split(',')]
    PRC_list = [float(x.strip('[]')) for x in sys.argv[5].split(',')]
    minor_axis = sys.argv[6]
    rotation_angle = sys.argv[7]
    PRC_colors_list = [float(x.strip('[]')) for x in sys.argv[8].split(',')]
    
    main(input_image, output_image, azimuth_list, elevation_list, PRC_list, minor_axis, rotation_angle, PRC_colors_list)
