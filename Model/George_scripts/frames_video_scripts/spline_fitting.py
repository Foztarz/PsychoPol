import numpy as np
import matplotlib.pyplot as plt
from scipy.interpolate import UnivariateSpline
from sklearn.metrics import r2_score
import sys

# Load data from the tab-separated file
data = np.loadtxt(sys.argv[1], delimiter='\t')
x = data[:, 0]
y = data[:, 1]

# Sort the x values and rearrange the corresponding y values
sorted_indices = np.argsort(x)
x = x[sorted_indices]
y = y[sorted_indices]

# Fit a spline to the sorted data
spline = UnivariateSpline(x, y)

# Print the coefficients of the spline
print("Spline Coefficients:", spline.get_coeffs())

# Generate x values for a smooth curve
x_smooth = np.linspace(min(x), max(x), 1000)

# Evaluate the spline at the smoothed x values
y_smooth = spline(x_smooth)

# Calculate R-squared (Note: R-squared calculation may not be as straightforward for splines)
r2 = r2_score(y, spline(x))

# Plot the results
plt.scatter(x, y, label="Labhart's data")
plt.plot(x_smooth, y_smooth, label=f'Spline Fit\nR-squared (approximate)={r2:.2f}', color='red')
plt.xlabel('position of stimulus (degrees)')
plt.ylabel('linear angular sensitivity')
plt.legend()
plt.show()
