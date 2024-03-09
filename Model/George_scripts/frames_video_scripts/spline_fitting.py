import sys
import numpy as np
from scipy.interpolate import UnivariateSpline
import matplotlib.pyplot as plt

# Check if the correct number of command-line arguments is provided
if len(sys.argv) != 2:
    print("Usage: python script.py <data_file>")
    sys.exit(1)

# Read data from the specified file
data_file = sys.argv[1]
data = np.loadtxt(data_file, delimiter='\t')

# Extract x and y columns
x = data[:, 0]
y = data[:, 1]

# Sort data in increasing order
sorted_indices = np.argsort(x)
x_sorted = x[sorted_indices]
y_sorted = y[sorted_indices]

# Specify the degree of the polynomial for the spline (cubic spline in this case)
k = 3

# Create a cubic spline object
spl = UnivariateSpline(x_sorted, y_sorted, s=0.015, k=k)
print(spl(50))
# Generate plot
# Set a new range of x values extending beyond x=100
x_range = np.linspace(x_sorted.min(), 110, 1000)

# Evaluate the spline on the new x range
ys = spl(x_range)

# Plot the original data points and the extended range of the spline curve
plt.plot(x_sorted, y_sorted, 'ro', ms=5)
plt.plot(x_range, ys, 'black', lw=5, alpha=0.3)
plt.xlim(x_sorted.min(), 110)  # Set x-axis limit to show values up to 110
plt.show()

