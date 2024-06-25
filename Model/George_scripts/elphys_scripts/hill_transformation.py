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
#relative_sensitivity = sensitivity_values / max_sensitivity_value

plt.figure(figsize=(8, 6))
plt.scatter(intensity_log, sensitivity_values, color='blue', label='Relative Sensitivity Values')
plt.xlabel('Intensity (log scale)')
plt.ylabel('Relative Sensitivity')
plt.title('Relative Sensitivity vs Intensity')
plt.grid(True)
plt.legend()
plt.tight_layout()
plt.show()


# to get local maxima from the spatial recording
command = f'python responses_localmaxima.py {sys.argv[2]}'  # Assuming sys.argv[2] is the argument for responses_localmaxima.py
output = subprocess.check_output(command, shell=True, text=True) # a 21 by 21 array with spatial sensitivity data
#print(output) # this is a string

output = output.replace('[', '').replace(']', '').replace('\n', ' ')

# Convert the cleaned string into a numpy array
output_values = np.fromstring(output, dtype=float, sep=' ').reshape(21, 21)

mV_ratio = np.max(output_values)/Emax_opt


if mV_ratio > 1.05:
    normalized_mV = output_values / float(np.max(output_values)) 
elif mV_ratio < 1:
    normalized_mV = output_values / float(Emax_opt) # yy in Gregor's script
else:
    normalized_mV = output_values / float(Emax_opt)
    np.where(normalized_mV > 1, 1, normalized_mV)


spatial_sensitivity = Khalf_opt * (normalized_mV / (1 - normalized_mV)) ** (1 / n_opt)
spatial_sensitivity = spatial_sensitivity / np.max(spatial_sensitivity)


plt.figure(figsize=(10, 8))
plt.imshow(normalized_mV, cmap='coolwarm', aspect='auto', vmin=0, vmax=1)
plt.colorbar(label='Relative Angular Sensitivity')
plt.title(f'Heatmap of Receptive Field {sys.argv[2]}')
plt.xlabel('x samples')
plt.ylabel('y samples')
plt.show()

    
    





