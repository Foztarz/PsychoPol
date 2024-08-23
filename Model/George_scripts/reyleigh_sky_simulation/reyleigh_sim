import numpy as np
from PIL import Image
import sys

def cartesian_to_spherical(x, y, center_x, center_y):
    azimuth_rad = np.arctan2(x - center_x, center_y - y)
    azimuth_deg = np.degrees(azimuth_rad) + 360  # +360 to make angles positive

    radius = np.sqrt((x - center_x)**2 + (y - center_y)**2)
    elevation_deg = 90 - 90 * radius / center_y  # elevation angle calculation
    if elevation_deg < 0:
        elevation_deg = 0
    return azimuth_deg % 360, elevation_deg

def calculate_aolp_sim(x1, y1, center_x, center_y, sun_x, sun_y):
    azimuth1, elevation1 = cartesian_to_spherical(x1, y1, center_x, center_y)
    azimuth2, elevation2 = cartesian_to_spherical(sun_x, sun_y, center_x, center_y)
    #print(azimuth2, elevation2)
    az1_rad = np.radians(azimuth1)
    el1_rad = np.radians(elevation1)
    az2_rad = np.radians(azimuth2)
    el2_rad = np.radians(elevation2)

    # cartesian coordinates for the sun & observation point
    Sx = np.cos(el2_rad) * np.cos(az2_rad)
    Sy = np.cos(el2_rad) * np.sin(az2_rad)
    Sz = np.sin(el2_rad)

    Ox = np.cos(el1_rad) * np.cos(az1_rad)
    Oy = np.cos(el1_rad) * np.sin(az1_rad)
    Oz = np.sin(el1_rad)

    # cross product to find the normal vector to the scattering plane
    Nx = Sy * Oz - Sz * Oy
    Ny = Sz * Ox - Sx * Oz
    Nz = Sx * Oy - Sy * Ox

    # azimuth of the normal vector in radians (AoLP)
    aolp_sim = np.arctan2(Ny, Nx)

    # normalized aolp_sim values to the range [0, 2*pi)
    if aolp_sim < 0:
        aolp_sim += 2 * np.pi

    return aolp_sim

def process_image(image_path, output_path):
    image = Image.open(image_path)
    width, height = image.size
    center_x, center_y = width // 2, height // 2

    sun_x, sun_y = 50, 841  # sun position, change accordingly

    with open(output_path, 'w') as file:
        for y in range(height):
            for x in range(width):
                aolp_sim = calculate_aolp_sim(x, y, center_x, center_y, sun_x, sun_y)
                file.write(f'{x}\t{y}\t{aolp_sim:.4f}\n')  # radians

if __name__ == "__main__":
    image_path = sys.argv[1]
    output_path = sys.argv[2]
    process_image(image_path, output_path)
