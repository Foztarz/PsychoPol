# this script prints the intensities of the pixels that correspond to the ommatidia of the second eye.

import sys
import numpy as np
import cv2
import scipy.stats
import matplotlib.pyplot as plt
from scipy.interpolate import UnivariateSpline



def spherical_to_cartesian(radius, azimuth_deg, elevation_deg): # this is for azimuthal equidistant projections
    s = radius * elevation_deg / 90  # distance from image edge
    azimuth_rad = np.radians(azimuth_deg)
    elevation_rad = np.radians(elevation_deg)
    x = int((radius - s) * np.sin(azimuth_rad))
    y = int(radius - (radius - s) * np.cos(azimuth_rad))
    return x, y

def cartesian_to_spherical(x, y, center_x, center_y):
    # Calculate azimuth angle in degrees
    azimuth_rad = np.arctan2(x - center_x, center_y - y)
    azimuth_deg = np.degrees(azimuth_rad) + 360 # +360 to make angles positive

    # Calculate elevation angle in degrees
    radius = np.sqrt((x - center_x)**2 + (y - center_y)**2)
    elevation_deg = 90-90 * radius / center_y # we add the '90-' to start counting from horizon
    return azimuth_deg, elevation_deg

def spherical_distance(x1, y1, x2, y2, center_x, center_y):
    # Convert Cartesian coordinates to spherical coordinates
    azimuth1, elevation1 = cartesian_to_spherical(x1, y1, center_x, center_y)
    azimuth2, elevation2 = cartesian_to_spherical(x2, y2, center_x, center_y)

    # Calculate spherical distance (haversine formula)
    delta_azimuth = np.radians(azimuth2 - azimuth1)
    delta_elevation = np.radians(elevation2 - elevation1)

    a = np.sin(delta_elevation / 2)**2 + np.cos(np.radians(elevation1)) * np.cos(np.radians(elevation2)) * np.sin(delta_azimuth / 2)**2
    c = 2 * np.arctan2(np.sqrt(a), np.sqrt(1 - a)) # central angle between the 2 points

    # Radius of the sphere
    radius = 1.0

    # Calculate the spherical distance
    distance = radius * c

    return distance # in radians

def main(spline_data, image_path, coordinates_file, minor_axis, rotation_angle):
    try:
        
        # first calculate the spline function that best fits the data
        # Read data from the specified file
        data_file = spline_data
        data = np.loadtxt(data_file, delimiter='\t')

        # Extract x and y columns
        x = data[:, 0]
        y = data[:, 1]

        # Sort data in increasing order
        sorted_indices = np.argsort(x)
        x_sorted = x[sorted_indices]
        y_sorted = y[sorted_indices]

        # Specify the degree of the polynomial for the spline (cubic spline in this case)
        k = 3

        # Create a cubic spline object
        spl = UnivariateSpline(x_sorted, y_sorted, s=0.015, k=k)
        
        # Open the circular image
        img = cv2.imread(image_path, cv2.IMREAD_GRAYSCALE)        
        
        # Calculate the center of the projection
        img_height, img_width = img.shape
        center_x = img_width // 2
        center_y = img_height // 2

        # this is for rotating the image if necessary (bicubic interpolation). Note that it rotates counterclockwise for positive angles
        M = cv2.getRotationMatrix2D((center_y,center_x),rotation_angle,1) # the format is cv2.getRotationMatrix2D(center, angle, scale) 
        img = cv2.warpAffine(img,M,(img_width,img_height),flags=cv2.INTER_CUBIC)
        
        # Read the coordinates from the file
        with open(coordinates_file, 'r') as file:
            for line in file:
                # Create a blank canvas with a black background
                canvas = np.zeros_like(img, dtype=np.uint8)
                
                azimuth_deg, elevation_deg = map(float, line.strip().split('\t'))

                # Calculate the pixel coordinates for the projection
                projection_radius = min(center_x, center_y)
                proj_x, proj_y = spherical_to_cartesian(projection_radius, azimuth_deg, elevation_deg)
                proj_x += center_x  # This is to set 0,0 to the north (top of the image)
                
                if proj_x >= center_x:
                    proj_x2 = proj_x - 2*(proj_x-center_x)
                elif proj_x < center_x:
                    proj_x2 = proj_x + 2*(center_x-proj_x)
                    
                # Create a distance matrix based on spline values
                x, y = np.meshgrid(np.arange(-center_x, img_width - center_x), np.arange(-center_y, img_height - center_y))
                distance_matrix = spherical_distance(x + center_x, y + center_y, proj_x2, proj_y, center_x, center_y)
                distance_matrix = np.degrees(distance_matrix)

                # Replace values greater than 40 with 40
                distance_matrix = np.where(distance_matrix > 50, 50, distance_matrix) # do this because the spline is weird and starts increasing after 50deg
                        
                spline_array = spl(distance_matrix)
                spline_array = np.where(spline_array > 1, 1, spline_array) # round down any value that might be above 1
                spline_array = np.where(spline_array < 0.0025, 0, spline_array) # round down any value that might be above below 0.0242 (50deg)
                # Set values outside the circular region to 0
                spline_array[(x)**2 + (y)**2 > center_x**2] = 0

                # Normalize the spline array to have a maximum value of 1
                #spline_array /= np.max(spline_array)
                
##                # Find the coordinates of the maximum value in the Gaussian matrix
##                max_coords = np.unravel_index(np.argmax(gaussian_array), gaussian_array.shape)
##
##                # Convert the coordinates back to pixel coordinates
##                max_pixel_x = max_coords[1] 
##                max_pixel_y = max_coords[0] 
##                # Print the coordinates
##                print(f"Max Value Coordinates: Pixel X = {max_pixel_x}, Pixel Y = {max_pixel_y}", proj_x,proj_y)

##                # Plot the Gaussian matrix
                #plt.imshow(spline_array, cmap='viridis', origin='upper')
                #plt.title(f'Gaussian Matrix - Ommatidium {azimuth_deg:.2f}, {elevation_deg:.2f}')
                #plt.colorbar()
                #plt.show()
                # this is for the second eye (mirrored)
                
                # Calculate the axes lengths for the ellipse and distortion based on azimuth
                minor_axis = int(minor_axis)
                if elevation_deg == 90: # this is for the unlikely case of 90deg elevation. Normally a limit has to be calculated.
                    distortion = 1
                else:
                    distortion = float(((np.pi/2) - np.pi * elevation_deg/180) / np.cos(np.pi * elevation_deg/180))

                major_axis = int(distortion * minor_axis)
                
                # Draw the rotated ellipse on the canvas
                thickness = -1  # -1 thickness fills the ellipse, thickness = 2 for transparent ellipses
                angle = -azimuth_deg  # Rotation angle in degrees (for the ellipses, not the image)
                cv2.ellipse(canvas, (proj_x2, proj_y), (major_axis, minor_axis), angle, 0, 360, 255, thickness) # 255 corresponds to white, canvas has now black and white pixels (ellipses are white)
                canvas[canvas == 255] = 1 # Convert 255 to 1 to multiply afterwards
                
                result = np.multiply(img, spline_array) # multiply the original img with the canvas (pixels that are outside of the ellipse are white on the canvas)
                #print(str(azimuth_deg) + '\t' +  str(elevation_deg) + '\t' + str(np.sum(result))) # prints the sum of intensities of each 'ommatiidum' image
                print(np.sum(result))
    except Exception as e:
        print(f"An error occurred: {str(e)}")

if __name__ == "__main__":
    if len(sys.argv) != 6:
        print("Usage: python script.py <spline_data> <input_image> <coordinates_file> <minor_axis> <rotation_angle>")
        sys.exit(1)

    spline_data = sys.argv[1]
    input_image = sys.argv[2]
    coordinates_file = sys.argv[3]
    minor_axis = sys.argv[4]
    rotation_angle = float(sys.argv[5])
    
    main(spline_data, input_image, coordinates_file, minor_axis, rotation_angle)
