# this script prints the intensities of the pixels that correspond to the ommatidia of the first eye, but using a gaussian relative sensitivity function.
# for usage check below
import sys
import numpy as np
import cv2
import scipy.stats
import matplotlib.pyplot as plt
from multiprocessing import Pool
import os

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

    
def generate_visibility_mask(lines, img, img_width, img_height, center_x, center_y, minor_axis, rotation_angle, sigma):
    visibility_mask = np.zeros_like(img, dtype=float)

    def process_point(azimuth_deg, elevation_deg):
        projection_radius = min(center_x, center_y)
        proj_x, proj_y = spherical_to_cartesian(projection_radius, azimuth_deg, elevation_deg)
        proj_x += center_x

        x, y = np.meshgrid(np.arange(-center_x, img_width - center_x), np.arange(-center_y, img_height - center_y))
        distance_matrix = spherical_distance(x + center_x, y + center_y, proj_x, proj_y, center_x, center_y)
        distance_matrix = np.degrees(distance_matrix)

        gaussian_array = scipy.stats.norm.pdf(distance_matrix, loc=0, scale=sigma)
        gaussian_array /= np.max(gaussian_array)
        gaussian_array = np.where(gaussian_array < 0.0025, 0, gaussian_array)
        gaussian_array[(x)**2 + (y)**2 > center_x**2] = 0

        return gaussian_array

    for line in lines:
        try:
            azimuth_deg, elevation_deg = map(float, line.strip().split('\t'))

            # Original point
            g_original = process_point(azimuth_deg, elevation_deg)

            # Mirrored point (along y-axis → flip azimuth)
            mirrored_azimuth = -azimuth_deg
            g_mirrored = process_point(mirrored_azimuth, elevation_deg)

            visibility_mask += g_original + g_mirrored

        except Exception as e:
            print(f"Error processing line: {line.strip()} | {str(e)}")

    masked_image = img * visibility_mask
    return masked_image, visibility_mask


def main(image_path, coordinates_file, minor_axis, rotation_angle, sigma):
    try:
        ext = os.path.splitext(image_path)[1].lower()
        if ext == ".npy":
            img = np.load(image_path)
        else:
            img = cv2.imread(image_path, cv2.IMREAD_GRAYSCALE)  # Loads as grayscale
            img = img.astype(np.float32) / 255.0  # Normalize to 0–1

        img_height, img_width = img.shape
        center_x = img_width // 2
        center_y = img_height // 2

        # Rotate image if needed
        M = cv2.getRotationMatrix2D((center_y, center_x), rotation_angle, 1)
        img = cv2.warpAffine(img, M, (img_width, img_height), flags=cv2.INTER_CUBIC)

        with open(coordinates_file, 'r') as file:
            lines = file.readlines()

        masked_image, visibility_mask = generate_visibility_mask(
            lines, img, img_width, img_height, center_x, center_y, int(minor_axis), rotation_angle, sigma
        )

        # Save outputs
        np.save("masked_image.npy", masked_image)
        np.save("visibility_mask.npy", visibility_mask)
        plt.imsave("masked_image.png", masked_image, cmap='gray')
        plt.imsave("visibility_mask.png", visibility_mask, cmap='gray')

        print("Masked image and visibility mask saved.")

    except Exception as e:
        print(f"An error occurred: {str(e)}")

if __name__ == "__main__":
    if len(sys.argv) != 6:
        print("Usage: python script.py <input_image> <coordinates_file> <minor_axis> <rotation_angle> <sigma>")
        sys.exit(1)

    input_image = sys.argv[1]
    coordinates_file = sys.argv[2]
    minor_axis = int(sys.argv[3])
    rotation_angle = float(sys.argv[4])
    sigma = float(sys.argv[5])

    main(input_image, coordinates_file, minor_axis, rotation_angle, sigma)
