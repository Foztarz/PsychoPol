import sys
import numpy as np
import cv2
from multiprocessing import Pool

def spherical_to_cartesian(radius, azimuth_deg, elevation_deg):
    """
    Convert spherical coordinates to Cartesian coordinates.
    Assumes azimuthal equidistant projection.
    """
    s = radius * elevation_deg / 90  # Distance from image edge
    azimuth_rad = np.radians(azimuth_deg)
    x = int((radius - s) * np.sin(azimuth_rad))
    y = int(radius - (radius - s) * np.cos(azimuth_rad))
    return x, y  # Cartesian coordinates

def process_line(args):
    """
    Process each coordinate to compute pixel intensities.
    """
    azimuth_deg, elevation_deg, img, img_width, img_height, center_x, center_y, minor_axis, rotation_angle = args
    
    projection_radius = min(center_x, center_y)
    proj_x, proj_y = spherical_to_cartesian(projection_radius, azimuth_deg, elevation_deg)
    proj_x += center_x  # Adjust to image center
    
    canvas = np.zeros_like(img, dtype=np.uint8)
    minor_axis = int(minor_axis)
    
    if elevation_deg == 90:  # Avoid division by zero
        distortion = 1
    else:
        distortion = ((np.pi / 2) - np.pi * elevation_deg / 180) / np.cos(np.pi * elevation_deg / 180)
    
    major_axis = int(distortion * minor_axis)
    thickness = -1  # Fill ellipse
    angle = -azimuth_deg  # Rotation angle for ellipse
    
    cv2.ellipse(canvas, (proj_x, proj_y), (major_axis, minor_axis), angle, 0, 360, 255, thickness)
    canvas[canvas == 255] = 1  # Set ellipse pixels to 1
    
    result = np.multiply(img, canvas)
    return np.sum(result)  # Sum of intensities

def main(image_path, azimuths, elevations, minor_axis, rotation_angle):
    """
    Main function to handle the processing of the image and coordinates.
    """
    try: 
        img = cv2.imread(image_path, cv2.IMREAD_GRAYSCALE)
        if img is None:
            raise FileNotFoundError(f"Image not found: {image_path}")
        
        img_height, img_width = img.shape
        center_x = img_width // 2
        center_y = img_height // 2
        
        M = cv2.getRotationMatrix2D((center_y, center_x), rotation_angle, 1)
        img = cv2.warpAffine(img, M, (img_width, img_height), flags=cv2.INTER_CUBIC)
        
        args_list = [(azimuth_deg, elevation_deg, img, img_width, img_height, center_x, center_y, minor_axis, rotation_angle) 
                     for azimuth_deg, elevation_deg in zip(azimuths, elevations)]
        
        with Pool() as pool:
            results = pool.map(process_line, args_list)
        
        for intensity in results:
            print(intensity)
    
    except Exception as e:
        print(f"An error occurred: {str(e)}")

if __name__ == "__main__":
    if len(sys.argv) < 6:
        print("Usage: python script.py <input_image> <azimuths> <elevations> <minor_axis> <rotation_angle>")
        print("Azimuths and elevations should be comma-separated lists, e.g., '30,60,90' '45,90,135'")
        sys.exit(1)
    
    input_image = sys.argv[1]
    azimuths = [float(x) for x in sys.argv[2].strip('[]').split(',')]
    elevations = [float(x) for x in sys.argv[3].strip('[]').split(',')]
    minor_axis = sys.argv[4]
    rotation_angle = float(sys.argv[5])
    
    

    if len(azimuths) != len(elevations):
        print("Error: Azimuth and elevation lists must be of the same length.")
        sys.exit(1)
    
    main(input_image, azimuths, elevations, minor_axis, rotation_angle)
