# this script prints the intensities of the pixels that correspond to the ommatidia of the first eye, but using a honeybee relative sensitivity function (based on Labhart 1980).
# for usage check below
import sys
import numpy as np
import cv2
import scipy.stats
import matplotlib.pyplot as plt
from scipy.interpolate import UnivariateSpline
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

    # radius of the sphere
    radius = 1.0

    # calculate the spherical distance
    distance = radius * c

    return distance # in radians

def process_line(args): # this function processes each line in the text file with the spherical coordinates (azimuth, elevation; tab-separated)
    line, img, spl, img_width, img_height, center_x, center_y, minor_axis = args # arguments, minor axis doesn't change in azimuthal equidistant projections, rotation angle refers to the ommatidia
    azimuth_deg, elevation_deg = map(float, line.strip().split('\t'))
    projection_radius = min(center_x, center_y) # radius of the circular input image
    proj_x, proj_y = spherical_to_cartesian(projection_radius, azimuth_deg, elevation_deg)
    proj_x += center_x # adding the radius of the image to bring 0 at the center
    # mirroring the coordinates of the second eye based on the first eye
    if proj_x >= center_x:
        proj_x2 = proj_x - 2*(proj_x-center_x)
    elif proj_x < center_x:
        proj_x2 = proj_x + 2*(center_x-proj_x)
    x, y = np.meshgrid(np.arange(-center_x, img_width - center_x), np.arange(-center_y, img_height - center_y))
    distance_matrix = spherical_distance(x + center_x, y + center_y, proj_x2, proj_y, center_x, center_y) # make a spherical-distance-matrix (distance between all pixels) assuming center of the surface of the sphere is zenith
    distance_matrix = np.degrees(distance_matrix)
    
    distance_matrix = np.where(distance_matrix > 50, 50, distance_matrix) # replace values greater than 50 with 50; do this for consistency with ephys data
    spline_array = spl(distance_matrix) # apply the spline function to the distance matrix
    spline_array = np.where(spline_array > 1, 1, spline_array) # make sensitivities greater than 1 equal to 1
    spline_array /= np.max(spline_array) # divides every element in the spline_array by the maximum value / normalization
    spline_array = np.where(spline_array < 0.0025, 0, spline_array) # round down any value that might be above below 0.0025 (50deg of the ephys data sensitivity)
    spline_array[(x)**2 + (y)**2 > center_x**2] = 0 # set values outside the circular region to 0
    
    result = np.multiply(img, spline_array) # multiply the original img with the spline array
    return np.sum(result) # get the intensity of all pixels covered by the ommatidium

def main(spline_data, image_path, coordinates_file, minor_axis, rotation_angle, threads):
    try:
        data = np.loadtxt(spline_data, delimiter='\t') # data to be used for the spline function (x/angles, y/sensitivity; tab-separated)
        x = data[:, 0]
        y = data[:, 1]
        sorted_indices = np.argsort(x)
        x_sorted = x[sorted_indices]
        y_sorted = y[sorted_indices]
        k = 3 # knots of the spline
        spl = UnivariateSpline(x_sorted, y_sorted, s=0.015, k=k) # s is the smoothing parameter
        img = np.load(image_path) # input image (has to be square, its center should be the center of the circular sky image)  
        img_height, img_width = img.shape
        center_x = img_width // 2
        center_y = img_height // 2
        M = cv2.getRotationMatrix2D((center_y,center_x),rotation_angle,1) # rotate the image
        img = cv2.warpAffine(img,M,(img_width,img_height),flags=cv2.INTER_CUBIC)
        
        with open(coordinates_file, 'r') as file:
            lines = file.readlines()
            args_list = [(line, img, spl, img_width, img_height, center_x, center_y, minor_axis) for line in lines] # parallel processing the process_line function for each ommatidium
            with Pool(processes=threads) as pool: # parallel processing the process_line function for each ommatidium
                results = pool.map(process_line, args_list)
               
            for intensity in results:
                print(intensity) # print every intensity
                
    except Exception as e:
        print(f"An error occurred: {str(e)}")

if __name__ == "__main__":
    if len(sys.argv) != 7:
        print("Usage: python script.py <spline_data> <input_image> <coordinates_file> <minor_axis> <rotation_angle> <threads>")
        sys.exit(1)
    spline_data = sys.argv[1]
    input_image = sys.argv[2]
    coordinates_file = sys.argv[3]
    minor_axis = sys.argv[4]
    rotation_angle = float(sys.argv[5])
    threads = int(sys.argv[6])
    main(spline_data, input_image, coordinates_file, minor_axis, rotation_angle, threads)
