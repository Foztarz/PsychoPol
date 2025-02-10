import numpy as np
import matplotlib.pyplot as plt
import sys

# Load data from the tab-separated file
data = np.loadtxt(sys.argv[1], delimiter='\t')

# Separate x and y values
x = data[:, 0]
y = data[:, 1]

# Perform linear regression
coefficients = np.polyfit(x, y, 1)  # 1 for linear regression, 2 for quadratic, etc.
poly = np.poly1d(coefficients)

# Print the equation of the line
print("Equation of the regression line: y = {:.4f}x + {:.4f}".format(coefficients[0], coefficients[1]))

# Generate points for the regression line
line_x = np.linspace(min(x), max(x), 100)
line_y = poly(line_x)

# Plot data and regression line
plt.scatter(x, y, label='Data')
plt.plot(line_x, line_y, color='red', label='Regression Line')
plt.xlabel('X')
plt.ylabel('Y')
plt.title('Linear Regression')
plt.legend()
plt.show()
