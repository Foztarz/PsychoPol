# this script prints the intensities of the pixels that correspond to the ommatidia of the first eye, but using a gaussian relative sensitivity function.
# for usage check below
import sys
import numpy as np
import cv2
import scipy.stats
import matplotlib.pyplot as plt
from multiprocessing import Pool

def spherical_to_cartesian(radius, azimuth_deg, elevation_deg): # turns spherical coordinates to cartesian, assuming azimuthal equidistant projection
    s = radius * elevation_deg / 90  # distance from image edge
    azimuth_rad = np.radians(azimuth_deg)
    elevation_rad = np.radians(elevation_deg)
    x = int((radius - s) * np.sin(azimuth_rad))
    y = int(radius - (radius - s) * np.cos(azimuth_rad))
    return x, y # cartesian coordinates

def cartesian_to_spherical(x, y, center_x, center_y): # turns cartesian coordinates to spherical, assuming azimuthal equidistant projection
    azimuth_rad = np.arctan2(x - center_x, center_y - y)
    azimuth_deg = np.degrees(azimuth_rad) + 360 # +360 to make angles positive

    radius = np.sqrt((x - center_x)**2 + (y - center_y)**2)
    elevation_deg = 90-90 * radius / center_y # we add the '90-' to start counting from horizon
    return azimuth_deg, elevation_deg

def spherical_distance(x1, y1, x2, y2, center_x, center_y): # calculates spherical distance between two points assuming center of the surface of the sphere is zenith
    azimuth1, elevation1 = cartesian_to_spherical(x1, y1, center_x, center_y)
    azimuth2, elevation2 = cartesian_to_spherical(x2, y2, center_x, center_y)

    # spherical distance (haversine formula)
    delta_azimuth = np.radians(azimuth2 - azimuth1)
    delta_elevation = np.radians(elevation2 - elevation1)

    a = np.sin(delta_elevation / 2)**2 + np.cos(np.radians(elevation1)) * np.cos(np.radians(elevation2)) * np.sin(delta_azimuth / 2)**2
    c = 2 * np.arctan2(np.sqrt(a), np.sqrt(1 - a)) # central angle between the 2 points
    
    radius = 1.0 # radius of the sphere
    
    distance = radius * c # calculate spherical distance

    return distance # in radians

    
def process_line(args): # this function processes each line in the text file with the spherical coordinates (azimuth, elevation; tab-separated)
    line, img, img_width, img_height, center_x, center_y, minor_axis, rotation_angle = args # arguments, minor axis doesn't change in azimuthal equidistant projections, rotation angle refers to the ommatidia
    
    try:
        azimuth_deg, elevation_deg = map(float, line.strip().split('\t'))

        projection_radius = min(center_x, center_y) # radius of the circular input image
        proj_x, proj_y = spherical_to_cartesian(projection_radius, azimuth_deg, elevation_deg)
        proj_x += center_x # adding the radius of the image to bring 0 at the center

        x, y = np.meshgrid(np.arange(-center_x, img_width - center_x), np.arange(-center_y, img_height - center_y)) # create 2-D gaussian array
        distance_matrix = spherical_distance(x + center_x, y + center_y, proj_x, proj_y, center_x, center_y) # make a spherical-distance-matrix (distance between all pixels) assuming center of the surface of the sphere is zenith
        distance_matrix = np.degrees(distance_matrix)
        
        distance_matrix = np.where(distance_matrix > 50, 50, distance_matrix) # replace values greater than 50 with 50; do this for consistency with ephys data
        
        sigma = 2.3184 # change this if different relative sensitivity, in degrees
        gaussian_array = scipy.stats.norm.pdf(distance_matrix, loc=0, scale=sigma) # location 0 to have the max value at the coordinates of the ommatidium
        gaussian_array = np.where(gaussian_array < 0.0025, 0, gaussian_array) # round down any value that might be above below 0.0025 (50deg of the ephys data sensitivity)
        
        gaussian_array[(x)**2 + (y)**2 > center_x**2] = 0 # set values outside the circular region to 0

        gaussian_array /= np.max(gaussian_array) # divides every element in the gaussian_array by the maximum value / normalization

        minor_axis = int(minor_axis)
        if elevation_deg == 90: # this is for the unlikely case of 90deg elevation. Normally a limit has to be calculated.
            distortion = 1
        else:
            distortion = float(((np.pi/2) - np.pi * elevation_deg/180) / np.cos(np.pi * elevation_deg/180)) # formula for distortion calculation in azimuthal equidistant projections

        major_axis = int(distortion * minor_axis)
                        
        result = np.multiply(img, gaussian_array) # multiply the original img with the gaussian array
        return np.sum(result) # get the intensity of all pixels covered by the ommatidium
    except Exception as e:
        print(f"An error occurred: {str(e)}")
        return 0

def main(image_path, coordinates_file, minor_axis, rotation_angle):
    try:
        img = cv2.imread(image_path, cv2.IMREAD_GRAYSCALE) # input image (has to be square, its center should be the center of the circular sky image)
        img_height, img_width = img.shape
        center_x = img_width // 2
        center_y = img_height // 2
        M = cv2.getRotationMatrix2D((center_y, center_x), rotation_angle, 1) # change this in case you want to rotate the image
        img = cv2.warpAffine(img, M, (img_width, img_height), flags=cv2.INTER_CUBIC)

        with open(coordinates_file, 'r') as file:
            lines = file.readlines()
            args_list = [(line, img, img_width, img_height, center_x, center_y, minor_axis, rotation_angle) for line in lines]
            with Pool() as pool: # parallel processing the process_line function for each ommatidium
                results = pool.map(process_line, args_list)
            for intensity in results:
                print(intensity) # print every intensity
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
