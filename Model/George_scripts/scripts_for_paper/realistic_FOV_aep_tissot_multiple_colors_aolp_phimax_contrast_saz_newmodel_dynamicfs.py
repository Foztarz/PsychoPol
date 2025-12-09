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

def main(image_path, output_path, azimuth_list, elevation_list, PRC_list, minor_axis, rotation_angle, PRC_colors_list, dynamicfs_list):
    try:

        # Open the circular image
        img = np.load(image_path)
        
        # Calculate the center of the projection
        img_height, img_width = img.shape
        center_x = img_width // 2
        center_y = img_height // 2

        # this is for rotating the image if necessary (bicubic interpolation). Note that it rotates counterclockwise for positive angles
        M = cv2.getRotationMatrix2D((center_y,center_x),0,1) # the format is cv2.getRotationMatrix2D(center, angle, scale) 
        img = cv2.warpAffine(img,M,(img_width,img_height),flags=cv2.INTER_CUBIC)

        # Create a blank canvas with a white background
        
        canvas = np.full((img.shape[0], img.shape[1], 3), (255, 255, 255), dtype=np.uint8) # create a 3rd channel for the image visualization, does not affect calculations
        
        # Initialize the total solar azimuth vector components (for all ommatidia of this eye)
        total_vector_x = 0
        total_vector_y = 0
        projection_radius = min(center_x, center_y)
        vectors_x = list() # per facet vectors
        vectors_y = list()
        for azimuth_deg, elevation_deg, PRC_value, PRC_color, dynamicfs in zip(azimuth_list, elevation_list, PRC_list, PRC_colors_list, dynamicfs_list):
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

            dynamicfs = np.degrees(float(dynamicfs))
            normalized_azimuth_deg = (azimuth_deg % 360 + 360) % 360
            normalized_phimax_deg = (dynamicfs % 360 + 360) % 360

            def ang_dist(a, b):
                return ((a - b + 180) % 360) - 180

            plus_90  = (normalized_phimax_deg + 90) % 360
            minus_90 = (normalized_phimax_deg - 90) % 360
            az180    = (normalized_azimuth_deg + 180) % 360

            # Compare angular distances
            if abs(ang_dist(plus_90, az180)) < abs(ang_dist(minus_90, az180)):
                # +90 is closer
                vector_x = abs(PRC_value) * np.sin(np.radians(normalized_phimax_deg+90))
                vector_y = abs(PRC_value) * np.cos(np.radians(normalized_phimax_deg+90))
            else:
                # -90 is closer
                vector_x = abs(PRC_value) * np.sin(np.radians(normalized_phimax_deg-90))
                vector_y = abs(PRC_value) * np.cos(np.radians(normalized_phimax_deg-90))
                
                
                


            # Draw arrow for each ommatidium
            arrow_scale = 300  # Adjust to control arrow length visually
            end_arrow_x = int(proj_x + arrow_scale * vector_x)
            end_arrow_y = int(proj_y - arrow_scale * vector_y)  # minus because y-axis is top-down in images

            # Set arrow color based on PRC value
            if PRC_value >= 0:
                arrow_color = (255, 0, 0)  # Red for positive PRC
            else:
                arrow_color = (0, 0, 255)  # Blue for negative PRC

            cv2.arrowedLine(
                canvas,
                (proj_x, proj_y),
                (end_arrow_x, end_arrow_y),
                color=arrow_color,
                thickness=2,
                tipLength=0.2  # relative size of the arrowhead
            )
            
            # Add the vector components to the total
            total_vector_x += vector_x
            total_vector_y += vector_y
            
            vectors_x.append(vector_x)
            vectors_y.append(vector_y)
            
            # Draw the rotated ellipse on the canvas, filling the ellipse with red color
            thickness = -1  # -1 thickness fills the ellipse, thickness = 2 for transparent ellipses
            angle = azimuth_deg  # Rotation angle in degrees (for the ellipses, not the image)
            #cv2.ellipse(canvas, (proj_x, proj_y), (major_axis, minor_axis), angle, 0, 360, rgba_color, thickness)

        # Calculate the end point of the total vector (normalized to the image size)
        total_vector_angle = float(math.atan2(float(total_vector_x), float(total_vector_y))) # atan2(x, y) because we're using sky coordinates (x axis is vertical) and not mathematical/geometrical (x axis is horizontal)
        total_vector_angle_ommatidia = [math.atan2(x, y) for x, y in zip(vectors_x, vectors_y)] # angle estimates for each ommatidium
        end_x = int(center_x + center_x * np.sin(total_vector_angle))
        end_y = int(center_x - center_x * np.cos(total_vector_angle)) # minus for the y coordinate because the center of the image is not 0,0
        print(total_vector_x, total_vector_y, total_vector_angle, total_vector_angle_ommatidia) # sending the components to the second eye
        # Draw the line on the canvas
        cv2.line(canvas, (center_x, center_y), (end_x, end_y), color=(0, 0, 255), thickness=2)
        
        canvas = cv2.cvtColor(canvas, cv2.COLOR_RGB2BGR)
        # Save the resulting image
        cv2.imwrite(output_path, canvas) # ACTIVATE TO SAVE IMAGE
        #print(f"Projection image saved to {output_path}")

    except Exception as e:
        print(f"An error occurred: {str(e)}")

if __name__ == "__main__":
    if len(sys.argv) != 10:
        print("Usage: python script.py <input_image> <output_image> <azimuth_list> <elevation_list> <PRC_list> <minor_axis> <rotation_angle> <PRC_colors_list> <dynamicfs_list>")
        sys.exit(1)

    input_image = sys.argv[1]
    output_image = sys.argv[2]
    azimuth_list = [float(x.strip('[]')) for x in sys.argv[3].split(',')]
    elevation_list = [float(x.strip('[]')) for x in sys.argv[4].split(',')]
    PRC_list = [float(x.strip('[]')) for x in sys.argv[5].split(',')]
    minor_axis = sys.argv[6]
    rotation_angle = sys.argv[7]
    PRC_colors_list = [float(x.strip('[]')) for x in sys.argv[8].split(',')]
    dynamicfs_list = [float(x.strip('[]')) for x in sys.argv[9].split(',')]
    
    main(input_image, output_image, azimuth_list, elevation_list, PRC_list, minor_axis, rotation_angle, PRC_colors_list, dynamicfs_list)
