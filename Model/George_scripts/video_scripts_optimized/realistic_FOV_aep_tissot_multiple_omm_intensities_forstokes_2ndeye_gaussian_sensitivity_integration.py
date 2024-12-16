# this script prints the intensities of the pixels that correspond to the ommatidia of the second eye, but using a gaussian relative sensitivity function.
# for usage check below
import sys
import numpy as np
import cv2
import scipy.stats
import matplotlib.pyplot as plt
from multiprocessing import Pool, Manager

def is_point_in_ellipse(px, py, cx, cy, a, b, angle_deg):
    angle_rad = np.radians(angle_deg)
    cos_angle = np.cos(angle_rad)
    sin_angle = np.sin(angle_rad)

    # translate point to ellipse center
    x = px - cx
    y = py - cy

    # rotate point relative to ellipse orientation
    x_rot = x * cos_angle + y * sin_angle
    y_rot = -x * sin_angle + y * cos_angle

    # check if the point lies within the ellipse
    return (x_rot**2 / a**2 + y_rot**2 / b**2) <= 1

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

# global variable to hold shared Gaussian arrays
global_gaussian_arrays = None

def init_worker(gaussian_arrays):
    """
    Initialize the global variable in each worker process.
    """
    global global_gaussian_arrays
    global_gaussian_arrays = gaussian_arrays

def precompute_gaussians(centers, img_width, img_height, center_x, center_y):
    """
    Precompute the Gaussian arrays for each ommatidium center and store them in a dictionary.
    
    Args:
        centers: List of (x, y) coordinates for ommatidia centers.
        img_width: Width of the image.
        img_height: Height of the image.
        center_x, center_y: Coordinates of the center of the image.
    
    Returns:
        A dictionary where the key is (center_x, center_y) and the value is the precomputed Gaussian array.
    """
    x, y = np.meshgrid(np.arange(-center_x, img_width - center_x), np.arange(-center_y, img_height - center_y))
    gaussian_arrays = {}  # store Gaussian arrays for each ommatidium; dictionary
    for raw_center in centers:
        center_x_pos, center_y_pos = raw_center
        distance_matrix = spherical_distance(x + center_x, y + center_y, center_x_pos, center_y_pos, center_x, center_y)
        distance_matrix = np.degrees(distance_matrix)
        #distance_matrix = np.where(distance_matrix > 50, 50, distance_matrix)  # cap distance at 50 degrees
        
        sigma = 2.3184  
        gaussian_array = scipy.stats.norm.pdf(distance_matrix, loc=0, scale=sigma)  # 2D Gaussian for ommatidium
        gaussian_array /= np.max(gaussian_array)  # normalize Gaussian
        gaussian_array = np.where(gaussian_array < 0.0025, 0, gaussian_array)  # sensitivity threshold
        gaussian_array[(x)**2 + (y)**2 > center_x**2] = 0  # set values outside the circular region to 0
        
        gaussian_arrays[(center_x_pos, center_y_pos)] = gaussian_array
    return gaussian_arrays

def process_line(args):
    """
    Processes a line containing spherical coordinates (azimuth, elevation) and calculates the total weighted intensity 
    for each ommatidium by summing its Gaussian response with the responses of its neighboring ommatidia.
    """
    global global_gaussian_arrays  # use the precomputed Gaussian arrays shared between processes
    
    line, img, img_width, img_height, center_x, center_y, minor_axis, rotation_angle, centers = args # arguments, minor axis doesn't change in azimuthal equidistant projections, rotation angle refers to the ommatidia
    try:
        azimuth_deg, elevation_deg = map(float, line.strip().split('\t'))
        projection_radius = min(center_x, center_y) # radius of the circular input image
        proj_x, proj_y = spherical_to_cartesian(projection_radius, azimuth_deg, elevation_deg)
        proj_x += center_x  # adding the radius of the image to bring 0 at the center
        
        if proj_x >= center_x: # mirroring the second eye's coordinates based on the first eye
            proj_x2 = proj_x - 2*(proj_x-center_x)
        elif proj_x < center_x:
            proj_x2 = proj_x + 2*(center_x-proj_x)

        minor_axis = int(minor_axis)
        minor_axis_whole = 42 # 9deg
        
        if elevation_deg == 90: # this is for the unlikely case of 90deg elevation. Normally a limit has to be calculated.
            distortion = 1
        else:
            distortion = float(((np.pi/2) - np.pi * elevation_deg/180) / np.cos(np.pi * elevation_deg/180)) # formula for distortion calculation in azimuthal equidistant projections

        major_axis = int(distortion * minor_axis)
        major_axis_whole = int(distortion * minor_axis_whole)
        thickness = -1 # fill 
        angle = -azimuth_deg # for rotation of the ommatidium
        
        # Gaussian array for the current ommatidium
        current_gaussian_array = global_gaussian_arrays.get((proj_x2, proj_y), np.zeros_like(img))
        
        # multiply image by the Gaussian array to compute intensity for the current ommatidium
        result = np.multiply(img, current_gaussian_array)
        
        # calculate the intensity for the current ommatidium
        current_intensity = np.sum(result)
        total_weighted_intensity = 0.58 * current_intensity  # Weight current ommatidium intensity 
                
        # calculate the intensities from neighboring ommatidia
        for raw_center in centers:
            neighbor_x, neighbor_y = raw_center
            if (neighbor_x, neighbor_y) != (proj_x2, proj_y) and is_point_in_ellipse(neighbor_x, neighbor_y, proj_x2, proj_y, major_axis_whole, minor_axis_whole, angle):
                neighbor_intensity = np.sum(np.multiply(img, global_gaussian_arrays.get((neighbor_x, neighbor_y), np.zeros_like(img))))
                total_weighted_intensity += 0.42 * neighbor_intensity  # weight neighboring intensities

        return total_weighted_intensity  # weighted intensity

    except Exception as e:
        print(f"An error occurred: {str(e)}")
        return 0

def main(image_path, coordinates_file, minor_axis, rotation_angle):
    try:

        img = cv2.imread(image_path, cv2.IMREAD_GRAYSCALE) # input image (has to be square, its center should be the center of the circular sky image)         
        
        img_height, img_width = img.shape
        center_x = img_width // 2
        center_y = img_height // 2

        # this is for rotating the image (bicubic interpolation). Note that it rotates counterclockwise for positive angles
        M = cv2.getRotationMatrix2D((center_y,center_x),rotation_angle,1) # the format is cv2.getRotationMatrix2D(center, angle, scale) 
        img = cv2.warpAffine(img,M,(img_width,img_height),flags=cv2.INTER_CUBIC)
        centers = []
        # Read the coordinates from the file
        with open(coordinates_file, 'r') as file:
            lines = file.readlines()
            for line in lines:
                azimuth_deg, elevation_deg = map(float, line.strip().split('\t'))
                projection_radius = min(center_x, center_y)
                proj_x, proj_y = spherical_to_cartesian(projection_radius, azimuth_deg, elevation_deg)
                proj_x += center_x
                if proj_x >= center_x: # mirroring the second eye's coordinates based on the first eye
                    proj_x2 = proj_x - 2*(proj_x-center_x)
                elif proj_x < center_x:
                    proj_x2 = proj_x + 2*(center_x-proj_x)
                centers.append((proj_x2, proj_y))
                
            # precompute the Gaussian arrays ONCE
            gaussian_arrays = precompute_gaussians(centers, img_width, img_height, center_x, center_y)
            
            args_list = [(line, img, img_width, img_height, center_x, center_y, minor_axis, rotation_angle, centers) for line in lines]
            with Pool(processes=10, initializer=init_worker, initargs=(gaussian_arrays,)) as pool: # parallel processing the process_line function for each ommatidium
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
