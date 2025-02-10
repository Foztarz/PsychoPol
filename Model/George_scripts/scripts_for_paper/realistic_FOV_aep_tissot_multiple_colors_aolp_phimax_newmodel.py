# this script makes a .csv file that has columns for azimuth, elevation, slope (slope of the line that is defined by the center of each ommatidium of the first eye and the center of the image),
# phi_max and phi_max_2 (phi_max + 90deg), phi max values are set to be perpendicular to each ommatidium's azimuth (Gkanias model) (slope is not actually needed in this script)

import sys
import numpy as np
import cv2
import matplotlib
import matplotlib.cm as cm
from multiprocessing import Pool

def spherical_to_cartesian(radius, azimuth_deg, elevation_deg):
    s = radius * elevation_deg / 90  
    azimuth_rad = np.radians(azimuth_deg)
    elevation_rad = np.radians(elevation_deg)
    x = int((radius - s) * np.sin(azimuth_rad))
    y = int(radius - (radius - s) * np.cos(azimuth_rad))
    return x, y

def process_pixel(args):
    azimuth_deg, elevation_deg, color_value, center_x, center_y, img_width, img_height, minor_axis = args

    projection_radius = min(center_x, center_y)
    proj_x, proj_y = spherical_to_cartesian(projection_radius, azimuth_deg, elevation_deg)
    proj_x += center_x  
    
    if proj_x == center_x:
        slope = np.inf if proj_y > center_y else -np.inf
    else:
        slope = (proj_y - center_y) / (proj_x - center_x)
    
    colormap = matplotlib.colormaps["hsv"]
    rgba_color = colormap(color_value)
    rgba_color = tuple(int(i * 255) for i in rgba_color)
    
    return azimuth_deg, elevation_deg, slope

def main(image_path, output_image, azimuth_list, elevation_list, color_value_list, minor_axis):
    try:
        img = cv2.imread(image_path)        
        img_height, img_width, _ = img.shape
        center_x = img_width // 2
        center_y = img_height // 2

        M = cv2.getRotationMatrix2D((center_y,center_x),0,1)
        img = cv2.warpAffine(img,M,(img_width,img_height),flags=cv2.INTER_CUBIC)

        pool = Pool()
        args_list = [(azimuth, elevation, color_value, center_x, center_y, img_width, img_height, minor_axis) 
                     for azimuth, elevation, color_value in zip(azimuth_list, elevation_list, color_value_list)]
        
        results = pool.map(process_pixel, args_list)
        pool.close()
        pool.join()

        data_array = np.zeros((len(results), 5))  # Initialize with 5 columns

        for i, (azimuth_deg, elevation_deg, slope) in enumerate(results):
            data_array[i, 0] = azimuth_deg
            data_array[i, 1] = elevation_deg
            data_array[i, 2] = slope

        data_array[:, 3] = -data_array[:, 0] + 90
        data_array[:, 4] = -data_array[:, 0]

        output_csv = "azimuth_elevation_slope_data.csv"
        np.savetxt(output_csv, data_array, delimiter="\t", comments="") 

    except Exception as e:
        print(f"An error occurred: {str(e)}")

if __name__ == "__main__":
    if len(sys.argv) != 7:
        print("Usage: python script.py <input_image> <output_image> <azimuth_list> <elevation_list> <color_value_list> <minor_axis>")
        sys.exit(1)

    input_image = sys.argv[1]
    output_image = sys.argv[2]
    azimuth_list = [float(x.strip('[]')) for x in sys.argv[3].split(',')]
    elevation_list = [float(x.strip('[]')) for x in sys.argv[4].split(',')]
    color_value_list = [float(x.strip('[]')) for x in sys.argv[5].split(',')]
    minor_axis = sys.argv[6]

    main(input_image, output_image, azimuth_list, elevation_list, color_value_list, minor_axis)

