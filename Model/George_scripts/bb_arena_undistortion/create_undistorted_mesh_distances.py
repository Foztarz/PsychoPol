import numpy as np

# Define grid size and cell size
rows, cols = 15, 15  # Set correct dimensions
cell_size = 50  # in mm

# Define the new center position
center_x = 7 * cell_size  # Column index 15 (14 left, 12 right)
center_y = 12 * cell_size  # Row index 14

# Initialize distance matrix
distance_matrix = np.zeros((rows, cols))

# Compute distances
for i in range(rows):
    for j in range(cols):
        x = j * cell_size
        y = i * cell_size
        distance_matrix[i, j] = np.sqrt((x - center_x) ** 2 + (y - center_y) ** 2)

# Save the matrix to a file
np.savetxt("distance_matrix.txt", distance_matrix, fmt="%.2f")

print("Distance matrix saved to 'distance_matrix.txt'")
