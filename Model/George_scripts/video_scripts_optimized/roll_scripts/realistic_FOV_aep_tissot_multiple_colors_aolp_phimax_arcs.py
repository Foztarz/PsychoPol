import sys
import cv2
import numpy as np
import matplotlib
import matplotlib.cm as cm
import colorsys
from multiprocessing import Pool

def spherical_to_cartesian(radius, azimuth_deg, elevation_deg):
    s = radius * elevation_deg / 90  
    azimuth_rad = np.radians(azimuth_deg)
    elevation_rad = np.radians(elevation_deg)
    x = int((radius - s) * np.sin(azimuth_rad))
    y = int(radius - (radius - s) * np.cos(azimuth_rad))
    return x, y

def fit_curve(x, y): 
    coefficients = np.polyfit(x, y, 2)
    curve = np.poly1d(coefficients)
    return curve

def process_pixel(args):
    azimuth_deg, elevation_deg, color_value, center_x, center_y, img_width, img_height, minor_axis = args

    projection_radius = min(center_x, center_y)
    proj_x, proj_y = spherical_to_cartesian(projection_radius, azimuth_deg, elevation_deg)
    proj_x += center_x  
    
    x_points = [0, img_width, proj_x]
    y_points = [center_y, img_height // 2, proj_y]

    curve = fit_curve(x_points, y_points)
    intercept = curve(center_x)
    
    if elevation_deg == 90: 
        distortion = 1
    else:
        distortion = float(((np.pi/2) - np.pi * elevation_deg/180) / np.cos(np.pi * elevation_deg/180))
    major_axis = int(float(distortion) * int(minor_axis))

    colormap = matplotlib.colormaps["hsv"]
    rgba_color = colormap(color_value)
    rgba_color = tuple(int(i * 255) for i in rgba_color)
    
    thickness = -1
    angle = azimuth_deg
    
    return azimuth_deg, elevation_deg, intercept

def main(image_path, output_path, azimuth_list, elevation_list, color_value_list, minor_axis):
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

        for i, (azimuth_deg, elevation_deg, intercept) in enumerate(results):
            data_array[i, 0] = azimuth_deg
            data_array[i, 1] = elevation_deg
            data_array[i, 2] = intercept

        min_intercept = np.min(data_array[:, 2])
        max_intercept = np.max(data_array[:, 2])
        max_phi_max = -0.4387 * int(min_intercept) + 470.8149 # this is to approximate Labhart 1980 data (basically the relationship that describes phi max-posteriority)
            
        data_array[:, 4] = np.interp(data_array[:, 2], [min_intercept, max_intercept], [float(max_phi_max), 0])
        data_array[:, 3] = data_array[:, 4] + 90

        np.savetxt("azimuth_elevation_arc_intercepts_data.csv", data_array, delimiter="\t", comments="")
        

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
