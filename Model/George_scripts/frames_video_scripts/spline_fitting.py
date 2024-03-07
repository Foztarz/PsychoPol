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

# Get the coefficients of the spline function
coefficients = spl.get_coeffs()

# Construct a polynomial using the coefficients
spline_function = np.poly1d(coefficients)

# Print the constructed polynomial
print("Spline Function:")
print(spline_function)

# Generate plot
xs = np.linspace(x_sorted.min(), x_sorted.max(), 100)
plt.plot(x_sorted, y_sorted, 'ro', ms=5)
plt.plot(xs, spl(xs), 'black', lw=5, alpha=0.3)
plt.show()
