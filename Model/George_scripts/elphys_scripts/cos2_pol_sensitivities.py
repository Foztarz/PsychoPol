import numpy as np
import matplotlib.pyplot as plt
from scipy.optimize import curve_fit
import sys

#  cosine squared function
def cosine_squared(x, A, B, C, D):
    return A * np.cos(B * x + C)**2 + D


data = np.loadtxt(sys.argv[1])  #  file with 2 columns: degrees, response
x_data = data[:, 0]
y_data = data[:, 1]

# initial parameters
initial_guess = [20, 0.018, 0, 0] # change A, B for better fits

# curve fitting
params, params_covariance = curve_fit(cosine_squared, x_data, y_data, p0=initial_guess, maxfev=10000)
print("Fitted parameters:", params)

# for a smooth curve
x_fine = np.linspace(min(x_data), max(x_data), 1000)  # more points for better accuracy

# fitted y values with optimized parameters
fitted_y_fine = cosine_squared(x_fine, *params)

max_y = np.max(fitted_y_fine)
min_y = np.min(fitted_y_fine)

x_at_max = x_fine[np.argmax(fitted_y_fine)]
x_at_min = x_fine[np.argmin(fitted_y_fine)]

print(max_y)
print(min_y)

plt.scatter(x_data, y_data, label='data', color='blue', s=10)
plt.plot(x_fine, fitted_y_fine, label='fitted cosine squared', color='red')

plt.scatter([x_at_max], [max_y], color='green', label='Max', zorder=5)
plt.scatter([x_at_min], [min_y], color='purple', label='Min', zorder=5)

plt.xlabel('degrees')
plt.ylabel('response (mV)')
plt.legend()
plt.show()
