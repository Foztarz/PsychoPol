# this script prints the intensities of the pixels that correspond to the ommatidia of the first eye.

import sys
import numpy as np
import cv2
import scipy.stats
import matplotlib.pyplot as plt

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

    
def main(image_path, coordinates_file, minor_axis, rotation_angle):
    try:

        # Open the circular image
        img = cv2.imread(image_path, cv2.IMREAD_GRAYSCALE)        
        
        # Calculate the center of the projection
        img_height, img_width = img.shape
        center_x = img_width // 2
        center_y = img_height // 2

            
        # this is for rotating the image if necessary (bicubic interpolation). Note that it rotates counterclockwise for positive angles
        M = cv2.getRotationMatrix2D((center_y,center_x),rotation_angle,1) # the format is cv2.getRotationMatrix2D(center, angle, scale) 
        img = cv2.warpAffine(img,M,(img_width,img_height),flags=cv2.INTER_CUBIC)

        # Create a blank canvas with a black background
        canvas = np.zeros_like(img, dtype=np.float64)

        # Read the coordinates from the file
        with open(coordinates_file, 'r') as file:
            for line in file:
                
                
                azimuth_deg, elevation_deg = map(float, line.strip().split('\t'))

                # Calculate the pixel coordinates for the projection
                projection_radius = min(center_x, center_y)
                proj_x, proj_y = spherical_to_cartesian(projection_radius, azimuth_deg, elevation_deg)
                proj_x += center_x  # This is to set 0,0 to the north (top of the image)

                # Create a 2D Gaussian array
                x, y = np.meshgrid(np.arange(-center_x, img_width - center_x), np.arange(-center_y, img_height - center_y))
                distance_matrix = spherical_distance(x + center_x, y + center_y, proj_x, proj_y, center_x, center_y)
                distance_matrix = np.degrees(distance_matrix)
                sigma = 2.3184 # change this if different relative sensitivity, in degrees
                gaussian_array = scipy.stats.norm.pdf(distance_matrix, loc=0, scale=sigma) # location 0 to have the max value at the coordinates of the ommatidium

                # Set values outside the circular region to 0
                gaussian_array[(x)**2 + (y)**2 > center_x**2] = 0

                
                # Normalize the Gaussian array to have a maximum value of 1
                #gaussian_array /= np.max(gaussian_array) # divides every element in the gaussian_array by the maximum value
                
##                # Find the coordinates of the maximum value in the Gaussian matrix
##                max_coords = np.unravel_index(np.argmax(gaussian_array), gaussian_array.shape)
##
##                # Convert the coordinates back to pixel coordinates
##                max_pixel_x = max_coords[1] 
##                max_pixel_y = max_coords[0] 
##                # Print the coordinates
##                print(f"Max Value Coordinates: Pixel X = {max_pixel_x}, Pixel Y = {max_pixel_y}", proj_x,proj_y)

                # Accumulate the Gaussian array to the canvas
                canvas += gaussian_array

                # Calculate the axes lengths for the ellipse and distortion based on azimuth
                minor_axis = int(minor_axis)
                if elevation_deg == 90: # this is for the unlikely case of 90deg elevation. Normally a limit has to be calculated.
                    distortion = 1
                else:
                    distortion = float(((np.pi/2) - np.pi * elevation_deg/180) / np.cos(np.pi * elevation_deg/180))

                major_axis = int(distortion * minor_axis)
                                
                result = np.multiply(img, gaussian_array) # multiply the original img with the canvas (pixels that are outside of the ellipse are white on the canvas)
                #print(str(azimuth_deg) + '\t' +  str(elevation_deg) + '\t' + str(np.sum(result))) # prints the sum of intensities of each 'ommatiidum' image
                print(np.sum(result))
                
            # Plot the Gaussian matrix
            # Mirror the canvas with respect to the vertical axis
            mirrored_canvas = np.flip(canvas, axis=1)

            # Add the mirror image to the existing canvas
            canvas = canvas + mirrored_canvas
            canvas /= np.max(canvas)
            #canvas = np.multiply(canvas, img) # multiply with the original image if desired

            # this shows the plot of the gaussians
            #plt.imshow(canvas, cmap='viridis', origin='upper')
            #plt.imshow(canvas, origin='upper')
            #plt.title('Combined Gaussian Matrix for all Ommatidia')
            #plt.colorbar()
            #plt.show()

            # Create a window to display the image
            #cv2.namedWindow('Canvas', cv2.WINDOW_NORMAL)

            # Display the canvas image
            #cv2.imshow('Canvas', canvas.astype(np.float64)/canvas.max()) # this is to make it less bright for cv2

            # Wait for a key event and close the window
            #cv2.waitKey(0)
            #cv2.destroyAllWindows()
            
    except Exception as e:
        print(f"An error occurred: {str(e)}")

if __name__ == "__main__":
    if len(sys.argv) != 5:
        print("Usage: python script.py <input_image> <coordinates_file> <minor_axis> <rotation_angle>")
        sys.exit(1)

    input_image = sys.argv[1]
    coordinates_file = sys.argv[2]
    minor_axis = sys.argv[3]
    rotation_angle = float(sys.argv[4])
    
    main(input_image, coordinates_file, minor_axis, rotation_angle)
