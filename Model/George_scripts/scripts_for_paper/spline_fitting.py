import sys
import numpy as np
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

# Sort data in decreasing order
sorted_indices = np.argsort(x)[::-1]  # Sort indices in decreasing order
x_sorted = x[sorted_indices]
y_sorted = y[sorted_indices]

# Fit a straight line (linear regression)
slope, intercept = np.polyfit(x_sorted, y_sorted, 1)

# Generate plot
# Set a new range of x values extending beyond x=100
x_range = np.linspace(x_sorted.min(), 110, 1000)

# Calculate y values for the line
y_line = slope * x_range + intercept

# Plot the original data points and the line
plt.plot(x_sorted, y_sorted, 'ro', ms=5)
plt.plot(x_range, y_line, 'black', lw=2)
plt.xlim(x_sorted.min(), 110)  # Set x-axis limit to show values up to 110
plt.xlabel('x')
plt.ylabel('y')
plt.title('Linear Regression')
plt.show()
