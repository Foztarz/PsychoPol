import numpy as np

def create_grid(image_size, num_points, center):
    # Calculate step size for both dimensions
    step_size_x = image_size[0] // int(np.sqrt(num_points))
    step_size_y = image_size[1] // int(np.sqrt(num_points))

    # Create a list of equally spaced pixels
    grid_points = []
    for x in range(0, image_size[0], step_size_x):
        for y in range(0, image_size[1], step_size_y):
            grid_points.append((x, y))

    # Convert grid points to spherical coordinates and filter out None values
    spherical_coordinates = [coord for coord in [cartesian_to_spherical(x, y, center[0], center[1]) for x, y in grid_points] if coord is not None]

    return spherical_coordinates


def cartesian_to_spherical(x, y, center_x, center_y):
    # Check if the point is within the circular area
    if np.sqrt((x - center_x)**2 + (y - center_y)**2) <= center_x:
        # Calculate azimuth angle in degrees
        azimuth_rad = np.arctan2(x - center_x, center_y - y)
        azimuth_deg = np.degrees(azimuth_rad) + 360  # +360 to make angles positive

        # Calculate elevation angle in degrees
        radius = np.sqrt((x - center_x)**2 + (y - center_y)**2)
        elevation_deg = 90 - 90 * radius / center_y  # we add the '90-' to start counting from the horizon

        return azimuth_deg, elevation_deg
    else:
        # Return None for points outside the circular area
        return None


def write_to_file(coordinates, filename):
    with open(filename, 'w') as file:
        for azimuth, elevation in coordinates:
            file.write(f"{azimuth}\t{elevation}\n")

# Image size (1683x1683), number of points (400), center coordinates, and output file name
image_size = (1683, 1683)
num_points = 400
center_coordinates = (image_size[0] // 2, image_size[1] // 2)
output_filename = "spherical_coordinates.txt"

# Create the grid and convert to spherical coordinates
spherical_coordinates = create_grid(image_size, num_points, center_coordinates)

# Write spherical coordinates to a file
write_to_file(spherical_coordinates, output_filename)

print(f"Spherical coordinates written to {output_filename}")
