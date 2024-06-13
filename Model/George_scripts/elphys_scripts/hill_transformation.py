import numpy as np
from scipy.optimize import curve_fit
import sys
import matplotlib.pyplot as plt
import subprocess

# Define the Hill equation
def hill_equation(I, Emax, Khalf, n):
    return Emax * (I**n) / (I**n + Khalf**n)

# Load data from text file
data = np.loadtxt(sys.argv[1])  

# Extract intensity and response values
intensity_log = data[:, 1]
response_values = data[:, 0]

# Convert intensity values from log scale to linear scale
intensity_linear = 10**intensity_log

# Initial parameter estimates
initial_guess = (30, 0.03, 2)  # Emax, K0.5, Hill slope

# Perform curve fitting using scipy.optimize.curve_fit
params, covariance = curve_fit(hill_equation, intensity_linear, response_values, p0=initial_guess)

# Extract the optimized parameters
Emax_opt, Khalf_opt, n_opt = params

# Print optimized parameters
print(f"Emax_opt = {Emax_opt}, K0.5_opt = {Khalf_opt}, n_opt = {n_opt}")

# Generate relative sensitivity values using the optimized parameters
sensitivity_values = hill_equation(intensity_linear, Emax_opt, Khalf_opt, n_opt)

# Normalize relative sensitivity values to range [0, 1]
max_sensitivity_value = np.max(sensitivity_values)
relative_sensitivity = sensitivity_values / max_sensitivity_value

# Plotting
plt.figure(figsize=(8, 6))
plt.scatter(intensity_log, relative_sensitivity, color='blue', label='Relative Sensitivity Values')
plt.xlabel('Intensity (log scale)')
plt.ylabel('Relative Sensitivity')
plt.title('Relative Sensitivity vs Intensity')
plt.grid(True)
plt.legend()
plt.tight_layout()
plt.show()

# Run subprocess to obtain additional response values (2D array)
command = f'python responses_localmaxima.py {sys.argv[2]}'  # Assuming sys.argv[2] is the argument for responses_localmaxima.py
output = subprocess.check_output(command, shell=True, text=True)



