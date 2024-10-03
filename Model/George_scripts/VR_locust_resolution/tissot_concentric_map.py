import cv2
import math
import numpy as np
import sys

def draw_tissot_pattern_cv2(input_img_path, output_img_path):
    img = cv2.imread(input_img_path)
    height, width, _ = img.shape

    center_x, center_y = width // 2, height // 2

    color = (0, 0, 255, 128)

    radius_step = 15  # circle every x pixels
    max_radius = min(center_x, center_y)  # maximum radius to fit the image

    # radial lines
    num_lines = 60
    angle_step = 360 / num_lines  # angle between lines

    minor_axis = 8  # initial minor axis for the Tissot ellipses
    
    elevation_scale = 180 / max_radius  # radius to elevation in degrees, the radius of the image is 180deg

    def draw_tissot_circle(img, x, y, radius, azimuth_deg):
        # elevation from the radius
        elevation_deg = 90 - (radius * elevation_scale)

        # distortion based on elevation
        if elevation_deg == 90:
            distortion = 1
        else:
            distortion = float(((np.pi / 2) - np.pi * elevation_deg / 180) / np.cos(np.pi * elevation_deg / 180))

        major_axis = int(distortion * minor_axis)

        angle = azimuth_deg  # rotation angle in degrees for the ellipse
        cv2.ellipse(
            img, 
            (int(x), int(y)),  # center of the ellipse
            (major_axis, minor_axis),  # major and minor axes
            angle,  # rotation angle
            0,  # start angle of the ellipse arc
            360,  # end angle of the ellipse arc
            color,  
            0  # transparent ellipse (thickness=-1 is filled)
        )

    # intersection points of the concentric circles and radial lines
    for radius in range(radius_step, max_radius, radius_step):
        for i in range(num_lines):
            angle = math.radians(i * angle_step)
            intersect_x = center_x + int(radius * math.cos(angle))
            intersect_y = center_y + int(radius * math.sin(angle))

            draw_tissot_circle(img, intersect_x, intersect_y, radius, i * angle_step + 90)

    # points on the diagonals of each square region
    for radius in range(radius_step, max_radius, radius_step):
        for i in range(num_lines):
            # current angle and next angle (for midpoints between lines)
            angle_current = math.radians(i * angle_step)
            angle_next = math.radians((i + 1) * angle_step)

            # midpoint angle between two radial lines
            angle_mid = (angle_current + angle_next) / 2

            # current radius and the next radius (for midpoints between circles)
            radius_next = radius + radius_step

            if radius_next < max_radius:
                mid_radius = (radius + radius_next) / 2

                mid_x = center_x + int(mid_radius * math.cos(angle_mid))
                mid_y = center_y + int(mid_radius * math.sin(angle_mid))

                draw_tissot_circle(img, mid_x, mid_y, mid_radius, math.degrees(angle_mid)+90)

    cv2.imwrite(output_img_path, img)

input_image_path = sys.argv[1]
output_image_path = sys.argv[2]
draw_tissot_pattern_cv2(input_image_path, output_image_path)
