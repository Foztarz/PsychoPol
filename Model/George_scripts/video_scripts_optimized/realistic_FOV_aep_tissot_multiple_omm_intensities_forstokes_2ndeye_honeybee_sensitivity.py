# this script prints the intensities of the pixels that correspond to the ommatidia of the second eye.

import sys
import numpy as np
import cv2
import scipy.stats
import matplotlib.pyplot as plt
from scipy.interpolate import UnivariateSpline
from multiprocessing import Pool


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

def process_line(args):
    line, img, spl, img_width, img_height, center_x, center_y, minor_axis = args
    azimuth_deg, elevation_deg = map(float, line.strip().split('\t'))
    projection_radius = min(center_x, center_y)
    proj_x, proj_y = spherical_to_cartesian(projection_radius, azimuth_deg, elevation_deg)
    proj_x += center_x
    if proj_x >= center_x:
        proj_x2 = proj_x - 2*(proj_x-center_x)
    elif proj_x < center_x:
        proj_x2 = proj_x + 2*(center_x-proj_x)
    x, y = np.meshgrid(np.arange(-center_x, img_width - center_x), np.arange(-center_y, img_height - center_y))
    distance_matrix = spherical_distance(x + center_x, y + center_y, proj_x2, proj_y, center_x, center_y)
    distance_matrix = np.degrees(distance_matrix)
    distance_matrix = np.where(distance_matrix > 50, 50, distance_matrix)
    spline_array = spl(distance_matrix)
    spline_array = np.where(spline_array > 1, 1, spline_array)
    spline_array = np.where(spline_array < 0.0025, 0, spline_array)
    spline_array[(x)**2 + (y)**2 > center_x**2] = 0
    spline_array /= np.max(spline_array)
    result = np.multiply(img, spline_array)
    return np.sum(result)

def main(spline_data, image_path, coordinates_file, minor_axis, rotation_angle):
    try:
        data = np.loadtxt(spline_data, delimiter='\t')
        x = data[:, 0]
        y = data[:, 1]
        sorted_indices = np.argsort(x)
        x_sorted = x[sorted_indices]
        y_sorted = y[sorted_indices]
        k = 3
        spl = UnivariateSpline(x_sorted, y_sorted, s=0.015, k=k)
        img = cv2.imread(image_path, cv2.IMREAD_GRAYSCALE)
        img_height, img_width = img.shape
        center_x = img_width // 2
        center_y = img_height // 2
        M = cv2.getRotationMatrix2D((center_y,center_x),rotation_angle,1)
        img = cv2.warpAffine(img,M,(img_width,img_height),flags=cv2.INTER_CUBIC)
        with open(coordinates_file, 'r') as file:
            lines = file.readlines()
            args_list = [(line, img, spl, img_width, img_height, center_x, center_y, minor_axis) for line in lines]
            with Pool() as pool:
                results = pool.map(process_line, args_list)
                
            for intensity in results:
                print(intensity)
                
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
