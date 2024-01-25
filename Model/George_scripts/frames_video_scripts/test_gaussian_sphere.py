import numpy as np
import matplotlib.pyplot as plt
from scipy.ndimage import gaussian_filter
import sys

# Function to create a Gaussian matrix with spherical distortion
def gaussian_matrix(shape, sigma, distortion_factor):
    x, y = np.meshgrid(np.arange(-shape[0]//2, shape[0]//2), np.arange(-shape[1]//2, shape[1]//2))
    x_distorted = x / distortion_factor
    y_distorted = y * distortion_factor
    gaussian = np.exp(-(x_distorted**2 + y_distorted**2) / (2 * sigma**2))
    gaussian /= np.sum(gaussian)
    return gaussian

# Function to apply Gaussian filter with spherical distortion to an image
def apply_gaussian_filter_with_distortion(image, sigma, distortion_factor):
    gaussian_kernel = gaussian_matrix(image.shape, sigma, distortion_factor)
    filtered_image = np.zeros_like(image, dtype=float)

    for i in range(image.shape[2]):  # Assuming image is a 3D array (height x width x channels)
        filtered_image[:, :, i] = gaussian_filter(image[:, :, i], sigma, mode='nearest', order=0)

    return filtered_image, gaussian_kernel

# Example usage
# Load an example image
image = plt.imread(sys.argv[1])

# Set parameters
sigma = 2.3184  # Standard deviation of the Gaussian kernel
distortion_factor = 5  # Adjust the distortion factor as needed

# Apply Gaussian filter with spherical distortion
filtered_image, gaussian_kernel = apply_gaussian_filter_with_distortion(image, sigma, distortion_factor)

# Display the original and filtered images
plt.figure()

plt.subplot(1, 3, 1)
plt.imshow(image)
plt.title('Original Image')

plt.subplot(1, 3, 2)
plt.imshow(filtered_image)
plt.title('Filtered Image with Spherical Distortion')

plt.subplot(1, 3, 3)
plt.imshow(gaussian_kernel, cmap='viridis', interpolation='none')
plt.title('Gaussian Kernel with Spherical Distortion')

plt.show()
