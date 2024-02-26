import numpy as np
import matplotlib.pyplot as plt
from scipy.interpolate import BSpline, splrep, splev
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



# Plot the results
plt.scatter(x, y, label="Labhart's data")
plt.plot(x_smooth, y_smooth, label=f'B-Spline Fit\nR-squared (approximate)={r2:.2f}', color='red')
plt.xlabel('position of stimulus (degrees)')
plt.ylabel('linear angular sensitivity')
plt.legend()
plt.show()
