import sys
import numpy as np
import cv2
from multiprocessing import Pool

def spherical_to_cartesian(radius, azimuth_deg, elevation_deg):
    s = radius * elevation_deg / 90  # distance from image edge
    azimuth_rad = np.radians(azimuth_deg)
    elevation_rad = np.radians(elevation_deg)
    x = int((radius - s) * np.sin(azimuth_rad))
    y = int(radius - (radius - s) * np.cos(azimuth_rad))
    return x, y

def process_line(args):
    line, img, img_width, img_height, center_x, center_y, minor_axis, rotation_angle = args
    azimuth_deg, elevation_deg = map(float, line.strip().split('\t'))
    projection_radius = min(center_x, center_y)
    proj_x, proj_y = spherical_to_cartesian(projection_radius, azimuth_deg, elevation_deg)
    proj_x += center_x
    if proj_x >= center_x:
        proj_x2 = proj_x - 2*(proj_x-center_x)
    elif proj_x < center_x:
        proj_x2 = proj_x + 2*(center_x-proj_x)
    canvas = np.zeros_like(img, dtype=np.uint8)
    minor_axis = int(minor_axis)
    if elevation_deg == 90:
        distortion = 1
    else:
        distortion = float(((np.pi/2) - np.pi * elevation_deg/180) / np.cos(np.pi * elevation_deg/180))
    major_axis = int(distortion * minor_axis)
    thickness = -1
    angle = -azimuth_deg
    cv2.ellipse(canvas, (proj_x2, proj_y), (major_axis, minor_axis), angle, 0, 360, 255, thickness)
    canvas[canvas == 255] = 1
    result = np.multiply(img, canvas)
    return np.sum(result)

def main(image_path, coordinates_file, minor_axis, rotation_angle):
    try:
        img = cv2.imread(image_path, cv2.IMREAD_GRAYSCALE)
        img_height, img_width = img.shape
        center_x = img_width // 2
        center_y = img_height // 2
        M = cv2.getRotationMatrix2D((center_y, center_x), rotation_angle, 1)
        img = cv2.warpAffine(img, M, (img_width, img_height), flags=cv2.INTER_CUBIC)
        with open(coordinates_file, 'r') as file:
            lines = file.readlines()
            args_list = [(line, img, img_width, img_height, center_x, center_y, minor_axis, rotation_angle) for line in lines]
            with Pool() as pool:
                results = pool.map(process_line, args_list)
            for intensity in results:
                print(intensity)
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
