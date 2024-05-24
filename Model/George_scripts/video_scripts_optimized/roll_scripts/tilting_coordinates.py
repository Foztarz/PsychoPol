import numpy as np
import os
import sys

def spherical_to_cartesian(azimuth, elevation):
    azimuth_rad = np.deg2rad(azimuth)
    elevation_rad = np.deg2rad(elevation)
    x = np.cos(elevation_rad) * np.cos(azimuth_rad)
    y = np.cos(elevation_rad) * np.sin(azimuth_rad)
    z = np.sin(elevation_rad)
    return np.array([x, y, z])

def cartesian_to_spherical(x, y, z):
    hxy = np.hypot(x, y)
    azimuth = np.rad2deg(np.arctan2(y, x))
    elevation = np.rad2deg(np.arctan2(z, hxy))
    return azimuth, elevation

def roll_rotation(points, roll_angle):
    roll_rad = np.deg2rad(roll_angle)
    rotation_matrix = np.array([
        [1, 0, 0],
        [0, np.cos(roll_rad), -np.sin(roll_rad)],
        [0, np.sin(roll_rad), np.cos(roll_rad)]
    ])
    rotated_points = np.dot(points, rotation_matrix.T)
    return rotated_points

def read_coordinates(filename):
    with open(filename, 'r') as file:
        lines = file.readlines()
    azimuths, elevations = [], []
    for line in lines:
        az, el = map(float, line.strip().split())
        azimuths.append(az)
        elevations.append(el)
    return azimuths, elevations

def write_coordinates(filename, azimuths, elevations):
    with open(filename, 'w') as file:
        for az, el in zip(azimuths, elevations):
            file.write(f"{az}\t{el}\n")

def process_file(filename):
    azimuths, elevations = read_coordinates(filename)
    cartesian_points = np.array([spherical_to_cartesian(az, el) for az, el in zip(azimuths, elevations)])
    
    roll_angles_positive = list(range(5, 50, 3))
    roll_angles_negative = list(range(-5, -50, -3))
    all_roll_angles = roll_angles_positive + roll_angles_negative

    for roll_angle in all_roll_angles:
        rotated_cartesian_points = roll_rotation(cartesian_points, roll_angle)
        new_spherical_points = [cartesian_to_spherical(*point) for point in rotated_cartesian_points]
        
        new_azimuths, new_elevations = zip(*new_spherical_points)
        
        base, ext = os.path.splitext(filename)
        new_filename = f"{base}_roll_{roll_angle}.txt"
        write_coordinates(new_filename, new_azimuths, new_elevations)

# Example usage:
if __name__ == "__main__":
    if len(sys.argv) > 1:
        process_file(sys.argv[1])
    else:
        print("Please provide a filename as an argument.")
