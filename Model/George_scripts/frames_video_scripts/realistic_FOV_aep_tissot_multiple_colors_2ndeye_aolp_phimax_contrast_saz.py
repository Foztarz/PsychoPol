# this script makes FOVs/ellipses with color that matches the PRC (photoreceptor contrast) values (bwr colormap) for the second eye

import sys
import numpy as np
import cv2
import matplotlib
import matplotlib.cm as cm
import colorsys
import math
from astropy.units import Quantity

def _components(data, p=1, phi=0.0, axis=None, weights=None):
    # Utility function for computing the generalized rectangular components
    # of the circular data.
    if weights is None:
        weights = np.ones((1,))
    try:
        weights = np.broadcast_to(weights, data.shape)
    except ValueError:
        raise ValueError("Weights and data have inconsistent shape.")

    C = np.sum(weights * np.cos(p * (data - phi)), axis) / np.sum(weights, axis)
    S = np.sum(weights * np.sin(p * (data - phi)), axis) / np.sum(weights, axis)

    return C, S


def _angle(data, p=1, phi=0.0, axis=None, weights=None):
    # Utility function for computing the generalized sample mean angle
    C, S = _components(data, p, phi, axis, weights)

    # theta will be an angle in the interval [-np.pi, np.pi)
    # [-180, 180)*u.deg in case data is a Quantity
    theta = np.arctan2(S, C)

    if isinstance(data, Quantity):
        theta = theta.to(data.unit)

    return theta

def circmean(data, axis=None, weights=None):
    return _angle(data, 1, 0.0, axis, weights)

def spherical_to_cartesian(radius, azimuth_deg, elevation_deg):
    s = radius * elevation_deg / 90  # distance from image edge
    azimuth_rad = np.radians(azimuth_deg)
    elevation_rad = np.radians(elevation_deg)
    x = int((radius - s) * np.sin(azimuth_rad))
    y = int(radius - (radius - s) * np.cos(azimuth_rad))
    return x, y

def main(image_path, output_path, azimuth_list, elevation_list, PRC_list, minor_axis, rotation_angle, first_eye_saz_x, first_eye_saz_y, PRC_colors_list):
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
        
        for azimuth_deg, elevation_deg, PRC_value, PRC_color in zip(azimuth_list, elevation_list, PRC_list, PRC_colors_list):
       
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
            rgba_color = colormap(PRC_color) # using scaled PRC values for colors
            rgba_color = tuple(int(i * 255) for i in rgba_color) # make to rgba

            # Calculate the vector components for each ommatidium
            vector_x = -PRC_value * np.cos(np.radians(azimuth_deg-float(rotation_angle))) # negative PRC value to match Evri's model (pointing to the inside), subtracting rotation angle to correct for body rotation
            vector_y = -PRC_value * np.sin(np.radians(azimuth_deg-float(rotation_angle))) # negative PRC value to match Evri's model (pointing to the inside), subtracting rotation angle to correct for body rotation

            # Add the vector components to the total
            total_vector_x += vector_x
            total_vector_y += vector_y
            
            # Draw the rotated ellipse on the canvas, filling the ellipse with red color
            thickness = -1  # -1 thickness fills the ellipse, thickness = 2 for transparent ellipses
            angle = -azimuth_deg  # Rotation angle in degrees (for the ellipses, not the image)
            cv2.ellipse(canvas, (proj_x2, proj_y), (major_axis, minor_axis), angle, 0, 360, rgba_color, thickness)

        # Calculate the end point of the total vector (normalized to the image size)
        total_vector_angle = math.atan2(total_vector_x, total_vector_y)
        total_vector_angle_botheyes = math.atan2(float(total_vector_x) + float(first_eye_saz_x), float(total_vector_y) + float(first_eye_saz_y)) # adding the components from the 1st eye
        end_x = int(center_x + center_x * np.cos(total_vector_angle)) # this is for the 2nd eye
        end_y = int(center_y + center_x * np.sin(total_vector_angle)) # this is for the 2nd eye
        end_x2 = int(center_x + center_x * np.cos(total_vector_angle_botheyes)) # this is for both eyes
        end_y2 = int(center_y + center_x * np.sin(total_vector_angle_botheyes)) # this is for both eyes
        
        #print('yes', total_vector_angle)
        # Draw the line on the canvas
        cv2.line(canvas, (center_x, center_y), (end_x, end_y), color=(255, 0, 0), thickness=2)
        cv2.line(canvas, (center_x, center_y), (end_x2, end_y2), color=(0, 255, 0), thickness=2) # combined saz
        
        canvas = cv2.cvtColor(canvas, cv2.COLOR_RGB2BGR)
        # Save the resulting image
        cv2.imwrite(output_path, canvas)
        #print(f"Projection image saved to {output_path}")

    except Exception as e:
        print(f"An error occurred: {str(e)}")

if __name__ == "__main__":
    if len(sys.argv) != 11:
        print("Usage: python script.py <input_image> <output_image> <azimuth_list> <elevation_list> <PRC_list> <minor_axis> <rotation_angle> <first_eye_saz_x> <first_eye_saz_y> <PRC_colors_list>")
        sys.exit(1)

    input_image = sys.argv[1]
    output_image = sys.argv[2]
    azimuth_list = [float(x.strip('[]')) for x in sys.argv[3].split(',')]
    elevation_list = [float(x.strip('[]')) for x in sys.argv[4].split(',')]
    PRC_list = [float(x.strip('[]')) for x in sys.argv[5].split(',')]
    minor_axis = sys.argv[6]
    rotation_angle = sys.argv[7]
    first_eye_saz_x = sys.argv[8]
    first_eye_saz_y = sys.argv[9]
    PRC_colors_list = [float(x.strip('[]')) for x in sys.argv[10].split(',')]
    
    main(input_image, output_image, azimuth_list, elevation_list, PRC_list, minor_axis, rotation_angle, first_eye_saz_x, first_eye_saz_y, PRC_colors_list)
