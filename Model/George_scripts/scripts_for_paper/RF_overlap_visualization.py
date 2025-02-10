import sys
import numpy as np
import cv2

def spherical_to_cartesian(radius, azimuth_deg, elevation_deg):
    s = radius * elevation_deg / 90  # distance from image edge
    azimuth_rad = np.radians(azimuth_deg)
    x = int((radius - s) * np.sin(azimuth_rad))
    y = int(radius - (radius - s) * np.cos(azimuth_rad))
    return x, y

def calculate_axes(elevation_deg, minor_axis):
    if elevation_deg == 90:  # edge case of 90 degrees elevation
        distortion = 1
    else:
        distortion = ((np.pi / 2) - np.pi * elevation_deg / 180) / np.cos(np.pi * elevation_deg / 180)
    major_axis = int(distortion * minor_axis)
    return minor_axis, major_axis

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

def main():
    txt_file = sys.argv[1]  # Text file with azimuth and elevation
    image_path = sys.argv[2]  # Path to the input image
    output_path = sys.argv[3]  # Path to save the output image
    
    image = cv2.imread(image_path)
    if image is None:
        print("Error: Image could not be loaded.")
        sys.exit(1)
    
    # assuming square image for radius calculation
    radius = min(image.shape[:2]) // 2
    
    points = []
    with open(txt_file, 'r') as file:
        for line in file:
            azimuth, elevation = map(float, line.strip().split('\t'))
            points.append((azimuth, elevation))
    
    cartesian_points = []
    center_x, center_y = radius, radius  # Center of the image
    for azimuth, elevation in points:
        x, y = spherical_to_cartesian(radius, azimuth, elevation)
        cartesian_points.append((x + center_x, y))
    
    # draw the ellipses and count points inside each ellipse
    minor_axis = 42
    total_count = 0
    ellipse_count = len(points)
    for i, (azimuth, elevation) in enumerate(points):
        cx, cy = cartesian_points[i]
        
        # ellipse axes
        minor, major = calculate_axes(elevation, minor_axis)
        
        # ellipse (angle is azimuth_deg for orientation)
        cv2.ellipse(image, (cx, cy), (major, minor), azimuth, 0, 360, (0, 0, 255), 1)
        cv2.circle(image, (cx, cy), 1, (0, 255, 0), -1)  # Radius 3, green color
        # count points inside the ellipse
        count = sum(1 for px, py in cartesian_points if is_point_in_ellipse(px, py, cx, cy, major, minor, azimuth))
        total_count += count-1
        # annotate the ellipse with the count-1 because it includes the own center
        cv2.putText(image, str(count-1), (cx + 5, cy - 5), 
                    cv2.FONT_HERSHEY_SIMPLEX, 0.5, (255, 255, 255), 1, cv2.LINE_AA)
        
    average_count = total_count / ellipse_count
    print(f"Average count of points within ellipses: {average_count}")
    
    cv2.imwrite(output_path, image)
    print(f"Image saved to {output_path}")

if __name__ == "__main__":
    main()
