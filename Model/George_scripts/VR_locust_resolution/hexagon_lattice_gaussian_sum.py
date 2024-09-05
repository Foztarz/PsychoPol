import numpy as np
import matplotlib.pyplot as plt
from matplotlib.patches import RegularPolygon
from PIL import Image
import sys

def gaussian_2d(x, y, mu_x, mu_y, sigma):
    """Compute the Gaussian weight for a point (x, y) with mean (mu_x, mu_y) and standard deviation sigma."""
    return (1 / (2 * np.pi * sigma ** 2)) * np.exp(-((x - mu_x) ** 2 + (y - mu_y) ** 2) / (2 * sigma ** 2))

def apply_gaussian_weights(image_array, hex_center, hex_size, sigma):
    """Apply a Gaussian kernel to the pixels inside the hexagon and return weighted sum of RGB values."""
    height, width, _ = image_array.shape
    cx, cy = hex_center
    
    # bounding box of the hexagon (estimate to limit the area)
    x_min = int(max(cx - hex_size, 0))
    x_max = int(min(cx + hex_size, width))
    y_min = int(max(cy - hex_size, 0))
    y_max = int(min(cy + hex_size, height))
    
    weighted_sum = np.zeros(3)  # RGB channels
    total_weight = 0
    
    for x in range(x_min, x_max):
        for y in range(y_min, y_max):
            # distance from the center of the hexagon
            distance = np.sqrt((x - cx) ** 2 + (y - cy) ** 2)
            
            # if the pixel is inside the hexagon, apply the Gaussian weight
            if distance <= hex_size:
                weight = gaussian_2d(x, y, cx, cy, sigma)
                weighted_sum += image_array[y, x, :] * weight  # multiply pixel value by weight
                total_weight += weight
    
    # normalize the result
    if total_weight > 0:
        weighted_sum /= total_weight
    
    return np.clip(weighted_sum, 0, 255).astype(int)

def create_hex_grid(ax, image_array, hex_size, color='red', linewidth=1, sigma=10, show_grid=True):
    """Create a hexagonal grid on the given axes and return hexagon center coordinates."""
    height, width, _ = image_array.shape
    hex_height = np.sqrt(3) * hex_size
    dx = 3 / 2 * hex_size
    dy = np.sqrt(3) * hex_size
    
    # number of hexagons needed
    nx = int(np.ceil(width / dx))
    ny = int(np.ceil(height / dy))
    
    centers = []
    
    for x in range(nx):
        for y in range(ny):
            cx = x * dx
            cy = y * dy
            
            if x % 2 == 1:
                cy += dy / 2
            
            centers.append((cx, cy))
            
            # apply Gaussian-weighted color for each hexagon
            color_rgb = apply_gaussian_weights(image_array, (cx, cy), hex_size, sigma)
            hex_color = color_rgb / 255.0  # Normalize to range [0, 1] for matplotlib
            
            # create a hexagon and fill it with the computed color
            hexagon = RegularPolygon((cx, cy), numVertices=6, radius=hex_size, orientation=np.pi / 6,
                                     edgecolor=color if show_grid else None,  # Show grid if requested
                                     linewidth=linewidth, facecolor=hex_color, fill=True)
            ax.add_patch(hexagon)
    
    return centers

def overlay_hexagonal_lattice(image_path, output_path_with_grid='output_with_grid.png',
                              output_path_without_grid='output_without_grid.png',
                              coord_output_path='hex_centers.txt', sigma=20):
    """Overlay a hexagonal lattice on an image, fill each hexagon with Gaussian-weighted pixel values, 
    and save the result with and without the grid."""
    image = Image.open(image_path)
    image_array = np.array(image)
    
    height, width, _ = image_array.shape

    # calculate the hexagon size based on image width
    hex_size = 5.5 * width / 100

    # image with the grid visible
    fig, ax = plt.subplots(figsize=(width/100, height/100), dpi=100)
    ax.imshow(image)

    hex_centers = create_hex_grid(ax, image_array, hex_size=hex_size, sigma=sigma, show_grid=True)

    ax.axis('off')
    plt.savefig(output_path_with_grid, bbox_inches='tight', pad_inches=0)
    plt.close(fig)

    # image without the grid visible
    fig, ax = plt.subplots(figsize=(width/100, height/100), dpi=100)
    ax.imshow(image)

    create_hex_grid(ax, image_array, hex_size=hex_size, sigma=sigma, show_grid=False)

    ax.axis('off')
    plt.savefig(output_path_without_grid, bbox_inches='tight', pad_inches=0)
    plt.close(fig)

    with open(coord_output_path, 'w') as f:
        for cx, cy in hex_centers:
            f.write(f"{cx}, {cy}\n")

if __name__ == "__main__":
    if len(sys.argv) != 4:
        print("Usage: python script.py <image_path> <output_with_grid> <output_without_grid>")
        sys.exit(1)
    
    image_path = sys.argv[1]
    output_with_grid = sys.argv[2]
    output_without_grid = sys.argv[3]
    
    overlay_hexagonal_lattice(image_path, output_path_with_grid=output_with_grid,
                              output_path_without_grid=output_without_grid)
