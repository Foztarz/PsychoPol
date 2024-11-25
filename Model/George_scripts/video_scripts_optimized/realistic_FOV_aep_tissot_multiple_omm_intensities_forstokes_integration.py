# this script prints the intensities of the pixels that correspond to the ommatidia of the first eye.
# for usage check below
import sys
import numpy as np
import cv2
from multiprocessing import Pool

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

def process_line(args): # this function processes each line in the text file with the spherical coordinates (azimuth, elevation; tab-separated)
    line, img, img_width, img_height, center_x, center_y, minor_axis, rotation_angle, centers = args # arguments, minor axis doesn't change in azimuthal equidistant projections, rotation angle refers to the ommatidia
    azimuth_deg, elevation_deg = map(float, line.strip().split('\t'))
    projection_radius = min(center_x, center_y) # radius of the circular input image
    proj_x, proj_y = spherical_to_cartesian(projection_radius, azimuth_deg, elevation_deg) # ommatidium's center cartesian coordinates
    proj_x += center_x # adding the radius of the image to bring 0 at the center
    canvas = np.zeros_like(img, dtype=np.uint8) # make a matrix full of 0s
    minor_axis = int(minor_axis)
    minor_axis_whole = 42 # 9deg
    if elevation_deg == 90: # unlikely case of elevation being exactly 90deg (normally a limit has to be calculated)
        distortion = 1
    else:
        distortion = float(((np.pi/2) - np.pi * elevation_deg/180) / np.cos(np.pi * elevation_deg/180)) # formula for distortion calculation in azimuthal equidistant projections
    major_axis = int(distortion * minor_axis)
    major_axis_whole = int(distortion * minor_axis_whole)
    thickness = -1 # fill 
    angle = azimuth_deg # for rotation of the ommatidium 
    cv2.ellipse(canvas, (proj_x, proj_y), (major_axis, minor_axis), angle, 0, 360, 255, thickness) # draw the ellipse (ommatidium)
    canvas[canvas == 255] = 1 # make ellipse pixels equal to 1
    result = np.multiply(img, canvas) # multiply original image with the matrix
    
    current_intensity = np.sum(result)
    total_weighted_intensity = 0.58 * current_intensity
    neighbors = []
    for raw_center in centers:
        neighbor_x, neighbor_y = raw_center

        if (neighbor_x, neighbor_y) != (proj_x, proj_y) and is_point_in_ellipse(neighbor_x, neighbor_y, proj_x, proj_y, major_axis_whole, minor_axis_whole, angle):
            # ellipse mask for the neighboring ommatidium
            neighbor_canvas = np.zeros_like(img, dtype=np.uint8)
            cv2.ellipse(neighbor_canvas, (int(neighbor_x), int(neighbor_y)), (major_axis, minor_axis), angle, 0, 360, 255, thickness)
            neighbor_canvas[neighbor_canvas == 255] = 1
            neighbor_result = np.multiply(img, neighbor_canvas)
            neighbor_intensity = np.sum(neighbor_result)
            neighbors.append(neighbor_intensity)
            
    num_neighbors = len(neighbors)
    if num_neighbors > 0:
        neighbor_weight = 0.42 / num_neighbors # this is the average weight (0.42) of our secondary RFs in our recordings
        for neighbor_intensity in neighbors:
            total_weighted_intensity += neighbor_weight * neighbor_intensity
    #print(num_neighbors)
    return total_weighted_intensity  # weighted intensity

def main(image_path, coordinates_file, minor_axis, rotation_angle):
    try: 
        img = cv2.imread(image_path, cv2.IMREAD_GRAYSCALE) # input image (has to be square, its center should be the center of the circular sky image)
        img_height, img_width = img.shape
        center_x = img_width // 2
        center_y = img_height // 2
        M = cv2.getRotationMatrix2D((center_y, center_x), rotation_angle, 1) # change this in case you want to rotate the image
        img = cv2.warpAffine(img, M, (img_width, img_height), flags=cv2.INTER_CUBIC)
        centers = []
        with open(coordinates_file, 'r') as file:
            lines = file.readlines()
            for line in lines:
                azimuth_deg, elevation_deg = map(float, line.strip().split('\t'))
                projection_radius = min(center_x, center_y)
                proj_x, proj_y = spherical_to_cartesian(projection_radius, azimuth_deg, elevation_deg)
                proj_x += center_x
                centers.append((proj_x, proj_y))
            args_list = [(line, img, img_width, img_height, center_x, center_y, minor_axis, rotation_angle, centers) for line in lines]
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
