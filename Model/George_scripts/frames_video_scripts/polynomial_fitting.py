import numpy as np
import matplotlib.pyplot as plt
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

# Use polyfit to fit a polynomial of degree 4 to the sorted data
coefficients = np.polyfit(x, y, 4)

# Print the coefficients of the polynomial fit
print("Polynomial Coefficients:", coefficients)

# Generate y values using the fitted polynomial coefficients
y_pred = np.polyval(coefficients, x)

# Calculate R-squared
r2 = r2_score(y, y_pred)

# Plot the results
plt.scatter(x, y, label="Labhart's data")
plt.plot(x, y_pred, label=f'Polynomial Regression (Degree=4)\nR-squared={r2:.2f}', color='red')
plt.xlabel('position of stimulus (degrees)')
plt.ylabel('linear angular sensitivity')
plt.legend()
plt.show()
