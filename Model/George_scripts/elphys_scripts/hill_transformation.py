import numpy as np
import scipy.ndimage
import matplotlib.pyplot as plt
from scipy.interpolate import interp2d
import sys
import subprocess
from scipy.optimize import curve_fit
from scipy.interpolate import griddata

# Hill equation
def hill_equation(I, Emax, Khalf, n):
    return Emax * (I**n) / (I**n + Khalf**n)

data = np.loadtxt(sys.argv[1])  

# intensity and response values
intensity_log = data[:, 1]
response_values = data[:, 0]

# intensity values from log scale to linear scale
intensity_linear = 10**intensity_log

# initial parameter estimates
initial_guess = (30, 0.03, 2)  # Emax, K0.5, Hill slope

# curve fitting using scipy.optimize.curve_fit
params, covariance = curve_fit(hill_equation, intensity_linear, response_values, p0=initial_guess)

# optimized parameters
Emax_opt, Khalf_opt, n_opt = params

print(f"Emax_opt = {Emax_opt}, K0.5_opt = {Khalf_opt}, n_opt = {n_opt}")

# generate relative sensitivity values using the optimized parameters
sensitivity_values = hill_equation(intensity_linear, Emax_opt, Khalf_opt, n_opt)

# normalize relative sensitivity values to range [0, 1]
max_sensitivity_value = np.max(sensitivity_values)
#relative_sensitivity = sensitivity_values / max_sensitivity_value

plt.figure(figsize=(8, 6))
plt.scatter(intensity_log, sensitivity_values, color='blue', label='Hill transformed responses')
plt.scatter(intensity_log, response_values, color='red', label='Response Values')
plt.xlabel('Intensity (log scale)')
plt.ylabel('Response')
plt.title('PR responses vs Intensity')
plt.grid(True)
plt.legend()
plt.tight_layout()
plt.show()


# to get local maxima from the spatial recording
command = f'python responses_localmaxima.py {sys.argv[2]}'  
output = subprocess.check_output(command, shell=True, text=True) # a 21 by 21 array with spatial sensitivity data
#print(output) # this is a string

output = output.replace('[', '').replace(']', '').replace('\n', ' ')

#convert cleaned string into a numpy array
output_values = np.fromstring(output, dtype=float, sep=' ').reshape(21, 21)

# smoothing methods
def bicubic_interpolation(data):
    x = np.arange(data.shape[1])
    y = np.arange(data.shape[0])
    x_new = np.linspace(0, data.shape[1] - 1, data.shape[1])
    y_new = np.linspace(0, data.shape[0] - 1, data.shape[0])
    X, Y = np.meshgrid(x, y)
    X_new, Y_new = np.meshgrid(x_new, y_new)
    data_bicubic = griddata((X.flatten(), Y.flatten()), data.flatten(), (X_new, Y_new), method='cubic')
    return data_bicubic

def gaussian_filtering(data, sigma):
    return scipy.ndimage.gaussian_filter(data, sigma=sigma)

def moving_average(data, window_size):
    return scipy.ndimage.uniform_filter(data, size=window_size)

bicubic_output = bicubic_interpolation(output_values)
gaussian_output = gaussian_filtering(output_values, sigma=0.75)
moving_average_output = moving_average(output_values, window_size=3)

def normalize_mV(smoothed_values, Emax_opt):
    mV_ratio = np.max(smoothed_values) / Emax_opt
    if mV_ratio > 1.05:
        normalized_mV = smoothed_values / float(np.max(smoothed_values))
    elif mV_ratio < 1:
        normalized_mV = smoothed_values / float(Emax_opt)
    else:
        normalized_mV = smoothed_values / float(Emax_opt)
        normalized_mV = np.where(normalized_mV > 1, 1, normalized_mV)
    return normalized_mV

bicubic_normalized_mV = normalize_mV(bicubic_output, Emax_opt)
gaussian_normalized_mV = normalize_mV(gaussian_output, Emax_opt)
moving_average_normalized_mV = normalize_mV(moving_average_output, Emax_opt)

# spatial sensitivity for each normalized result
def compute_spatial_sensitivity(normalized_mV, Khalf_opt, n_opt):
    spatial_sensitivity = Khalf_opt * (normalized_mV / (1 - normalized_mV)) ** (1 / n_opt)
    return spatial_sensitivity / np.max(spatial_sensitivity)

bicubic_sensitivity = compute_spatial_sensitivity(bicubic_normalized_mV, Khalf_opt, n_opt)
gaussian_sensitivity = compute_spatial_sensitivity(gaussian_normalized_mV, Khalf_opt, n_opt)
moving_average_sensitivity = compute_spatial_sensitivity(moving_average_normalized_mV, Khalf_opt, n_opt)

# plot the results
plt.figure(figsize=(15, 10))

plt.subplot(2, 2, 1)
plt.imshow(bicubic_sensitivity, cmap='coolwarm', aspect='auto', vmin=0, vmax=1)
plt.title('Bicubic Interpolation Sensitivity')
plt.colorbar()

plt.subplot(2, 2, 2)
plt.imshow(gaussian_sensitivity, cmap='coolwarm', aspect='auto', vmin=0, vmax=1)
plt.title('Gaussian Filtering Sensitivity')
plt.colorbar()

plt.subplot(2, 2, 3)
plt.imshow(moving_average_sensitivity, cmap='coolwarm', aspect='auto', vmin=0, vmax=1)
plt.title('Moving Average Sensitivity')
plt.colorbar()

plt.tight_layout()
plt.show()

    
    





