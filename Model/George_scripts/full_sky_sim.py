import numpy as np
from PIL import Image
import sys
import matplotlib.colors as mcolors
import os

def spherical_to_cartesian(radius, azimuth_deg, elevation_deg): # turns spherical coordinates to cartesian, assuming azimuthal equidistant projection
    s = radius * elevation_deg / 90  # distance from image edge
    azimuth_rad = np.radians(azimuth_deg)
    elevation_rad = np.radians(elevation_deg)
    x = int((radius - s) * np.sin(azimuth_rad))
    y = int(radius - (radius - s) * np.cos(azimuth_rad))
    #print(x,y)
    return x, y # cartesian coordinates

def cartesian_to_spherical(x, y, center_x, center_y):
    azimuth_rad = np.arctan2(x - center_x, center_y - y)
    azimuth_deg = np.degrees(azimuth_rad) + 360  # +360 to make angles positive

    radius = np.sqrt((x - center_x)**2 + (y - center_y)**2)
    elevation_deg = 90 - 90 * radius / center_y  # elevation angle calculation
    if elevation_deg < 0:
        elevation_deg = 0
    return azimuth_deg % 360, elevation_deg

import numpy as np

def calculate_stokes(x1, y1, center_x, center_y, sun_x, sun_y):
    """
    Compute AoLP, DoP, and Stokes parameters (S0, S1, S2) for a single pixel.

    Returns:
        aolp_sim: angle of linear polarization (radians)
        dop_sim: degree of linear polarization [0-1]
        S0: total intensity (arbitrary units)
        S1: first Stokes parameter
        S2: second Stokes parameter
    """
    # Convert pixel to spherical coordinates
    azimuth1, elevation1 = cartesian_to_spherical(x1, y1, center_x, center_y)
    
    # Sun position (hardcoded like before, modify if needed)
    azimuth2, elevation2 = -108, 67
    
    az1_rad = np.radians(azimuth1)
    el1_rad = np.radians(elevation1)
    az2_rad = np.radians(azimuth2)
    el2_rad = np.radians(elevation2)

    # 3D cartesian vectors
    Sx = np.cos(el2_rad) * np.cos(az2_rad)
    Sy = np.cos(el2_rad) * np.sin(az2_rad)
    Sz = np.sin(el2_rad)

    Ox = np.cos(el1_rad) * np.cos(az1_rad)
    Oy = np.cos(el1_rad) * np.sin(az1_rad)
    Oz = np.sin(el1_rad)

    # --- AoLP ---
    Nx = Sy * Oz - Sz * Oy
    Ny = Sz * Ox - Sx * Oz
    Nz = Sx * Oy - Sy * Ox
    aolp_sim = np.mod(-np.arctan2(Ny, Nx) + np.pi/2, np.pi)

    # --- DoP (Rayleigh) ---
    dot = Sx*Ox + Sy*Oy + Sz*Oz
    theta = np.arccos(np.clip(dot, -1.0, 1.0))  # scattering angle
    dop_sim = (np.sin(theta)**2) / (1 + np.cos(theta)**2)
    dop_sim = np.clip(dop_sim, 0, 1)

    # --- Intensity S0 ---
    S0 = 1.0 * (1 + np.cos(theta)**2)  # simple Rayleigh intensity
    # optional horizon darkening:
    # S0 *= np.cos(el1_rad)**1.5

    # --- Stokes parameters ---
    S1 = S0 * dop_sim * np.cos(2 * aolp_sim)
    S2 = S0 * dop_sim * np.sin(2 * aolp_sim)

    return aolp_sim, dop_sim, S0, S1, S2



def process_image(image_path, output_path):
    image = Image.open(image_path)
    width, height = image.size
    center_x, center_y = width // 2, height // 2

    sun_x, sun_y = spherical_to_cartesian(center_x, -288, 1)  # sun position, change accordingly; 0 is up 90 is right
    #print(sun_x, sun_y)
    with open(output_path, 'w') as file:
        for y in range(height):
            for x in range(width):
                aolp, dop, S0, S1, S2 = calculate_stokes(x, y, center_x, center_y, sun_x, sun_y)
                file.write(f'{x}\t{y}\t{aolp:.4f}\t{dop:.4f}\t{S0:.4f}\t{S1:.4f}\t{S2:.4f}\n')



if __name__ == "__main__":
    image_path = sys.argv[1]  # input image to get size
    output_txt = sys.argv[2]  # save all values
    output_aolp_image = sys.argv[3]
    output_dop_image = sys.argv[4]
    output_intensity_image = sys.argv[5]
    output_npy_folder = sys.argv[6]  # folder to save polarizer npy files

    os.makedirs(output_npy_folder, exist_ok=True)

    # Open image for size
    image = Image.open(image_path)
    width, height = image.size
    center_x, center_y = width // 2, height // 2

    # Sun position
    sun_x, sun_y = spherical_to_cartesian(center_x, -288, 1)

    # Prepare arrays
    aolp_image_array = np.zeros((height, width, 3))
    dop_image_array = np.zeros((height, width))
    intensity_image_array = np.zeros((height, width))
    stokes_array = np.zeros((height, width, 3))  # store S0, S1, S2 for polarizer calculations

    # Save values
    with open(output_txt, 'w') as file:
        file.write('x\ty\taolp\tdop\tS0\tS1\tS2\n')
        for y in range(height):
            for x in range(width):
                aolp, dop, S0, S1, S2 = calculate_stokes(x, y, center_x, center_y, sun_x, sun_y)
                file.write(f'{x}\t{y}\t{aolp:.4f}\t{dop:.4f}\t{S0:.4f}\t{S1:.4f}\t{S2:.4f}\n')

                # Store Stokes for polarizer computation
                stokes_array[y, x] = [S0, S1, S2]

                # AoLP image (hue)
                hsv_color = np.array([(aolp % np.pi)/np.pi, 1.0, 1.0])
                aolp_image_array[y, x] = mcolors.hsv_to_rgb(hsv_color)

                # DoP image (grayscale)
                dop_image_array[y, x] = dop

                # Intensity image (grayscale)
                intensity_image_array[y, x] = S0

    # Save images
    Image.fromarray((aolp_image_array * 255).astype(np.uint8)).save(output_aolp_image)
    Image.fromarray((dop_image_array * 255).astype(np.uint8)).save(output_dop_image)
    S0_normalized = (intensity_image_array - intensity_image_array.min()) / (intensity_image_array.max() - intensity_image_array.min())
    Image.fromarray((S0_normalized * 255).astype(np.uint8)).save(output_intensity_image)

    # --- Save polarizer images as .npy ---
    polarizer_angles_deg = [0, 45, 90, 135]
    for angle_deg in polarizer_angles_deg:
        angle_rad = np.radians(angle_deg)
        I_phi = 0.5 * (stokes_array[:, :, 0] + stokes_array[:, :, 1] * np.cos(2*angle_rad) + stokes_array[:, :, 2] * np.sin(2*angle_rad))
        np.save(os.path.join(output_npy_folder, f'polarizer_{angle_deg}.npy'), I_phi)

