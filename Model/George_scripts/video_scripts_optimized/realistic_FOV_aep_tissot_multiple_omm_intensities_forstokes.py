# this script prints the intensities of the pixels that correspond to the ommatidia of the first eye.
# for usage check below
import sys
import numpy as np
import cv2
from multiprocessing import Pool

def spherical_to_cartesian(radius, azimuth_deg, elevation_deg): # turns spherical coordinates to cartesian, assuming azimuthal equidistant projection
    s = radius * elevation_deg / 90  # distance from image edge
    azimuth_rad = np.radians(azimuth_deg)
    elevation_rad = np.radians(elevation_deg)
    x = int((radius - s) * np.sin(azimuth_rad))
    y = int(radius - (radius - s) * np.cos(azimuth_rad))
    return x, y # cartesian coordinates

def process_line(args): # this function processes each line in the text file with the spherical coordinates (azimuth, elevation; tab-separated)
    line, img, img_width, img_height, center_x, center_y, minor_axis, rotation_angle = args # arguments, minor axis doesn't change in azimuthal equidistant projections, rotation angle refers to the ommatidia
    azimuth_deg, elevation_deg = map(float, line.strip().split('\t'))
    projection_radius = min(center_x, center_y) # radius of the circular input image
    proj_x, proj_y = spherical_to_cartesian(projection_radius, azimuth_deg, elevation_deg) # ommatidium's center cartesian coordinates
    proj_x += center_x # adding the radius of the image to bring 0 at the center
    canvas = np.zeros_like(img, dtype=np.uint8) # make a matrix full of 0s
    minor_axis = int(minor_axis)
    if elevation_deg == 90: # unlikely case of elevation being exactly 90deg (normally a limit has to be calculated)
        distortion = 1
    else:
        distortion = float(((np.pi/2) - np.pi * elevation_deg/180) / np.cos(np.pi * elevation_deg/180)) # formula for distortion calculation in azimuthal equidistant projections
    major_axis = int(distortion * minor_axis)
    thickness = -1 # fill 
    angle = azimuth_deg # for rotation of the ommatidium 
    cv2.ellipse(canvas, (proj_x, proj_y), (major_axis, minor_axis), angle, 0, 360, 255, thickness) # draw the ellipse (ommatidium)
    canvas[canvas == 255] = 1 # make ellipse pixels equal to 1
    result = np.multiply(img, canvas) # multiply original image with the matrix
    return np.sum(result) # get the intensity of all pixels covered by the ommatidium

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
