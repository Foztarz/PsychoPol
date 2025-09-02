import sys
import numpy as np
import cv2
import math

def spherical_to_cartesian(radius, azimuth_deg, elevation_deg):
    s = radius * elevation_deg / 90  # distance from image edge
    azimuth_rad = np.radians(azimuth_deg)
    elevation_rad = np.radians(elevation_deg)
    x = int((radius - s) * np.sin(azimuth_rad))
    y = int(radius - (radius - s) * np.cos(azimuth_rad))
    return x, y

def load_azimuth_elevation_from_tsv(tsv_path):
    azimuths = []
    elevations = []
    with open(tsv_path, 'r') as f:
        for line in f:
            parts = line.strip().split('\t')
            if len(parts) != 2:
                continue
            try:
                azimuths.append(float(parts[0]))
                elevations.append(float(parts[1]))
            except ValueError:
                continue
    return azimuths, elevations

def main(image_path, output_path, tsv_path):
    try:
        azimuth_list, elevation_list = load_azimuth_elevation_from_tsv(tsv_path)

        img = cv2.imread(image_path)
        img_height, img_width, _ = img.shape
        center_x = img_width // 2
        center_y = img_height // 2

        # Optional rotation
        M = cv2.getRotationMatrix2D((center_y, center_x), 0, 1)
        img = cv2.warpAffine(img, M, (img_width, img_height), flags=cv2.INTER_CUBIC)

        # White canvas
        canvas = np.zeros((img_height, img_width, 4), dtype=np.uint8)

        for azimuth_deg, elevation_deg in zip(azimuth_list, elevation_list):
            for mirrored_azimuth in [azimuth_deg, -azimuth_deg]:
                projection_radius = min(center_x, center_y)
                proj_x, proj_y = spherical_to_cartesian(projection_radius, mirrored_azimuth, elevation_deg)
                proj_x += center_x

                # Perpendicular line to azimuth (use the mirrored azimuth)
                angle_rad = np.radians(mirrored_azimuth+90)
                line_length = 15
                x1 = int(proj_x - line_length * np.cos(angle_rad))
                y1 = int(proj_y - line_length * np.sin(angle_rad))
                x2 = int(proj_x + line_length * np.cos(angle_rad))
                y2 = int(proj_y + line_length * np.sin(angle_rad))

                # Light blue in BGR
                light_blue = (150,150, 255,255)

                cv2.line(canvas, (x1, y1), (x2, y2), color=light_blue, thickness=5)


        canvas = cv2.cvtColor(canvas, cv2.COLOR_RGB2BGR)
        cv2.imwrite(output_path, canvas)

    except Exception as e:
        print(f"An error occurred: {str(e)}")

if __name__ == "__main__":
    if len(sys.argv) != 4:
        print("Usage: python script.py <input_image> <output_image> <azimuth_elevation_tsv>")
        sys.exit(1)

    input_image = sys.argv[1]
    output_image = sys.argv[2]
    tsv_file = sys.argv[3]

    main(input_image, output_image, tsv_file)
