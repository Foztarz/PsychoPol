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

##bicubic_output = bicubic_interpolation(output_values)
gaussian_output = gaussian_filtering(output_values, sigma=1)
moving_average_output = moving_average(output_values, window_size=3)

##bicubic_normalized_mV = normalize_mV(bicubic_output, Emax_opt)
gaussian_normalized_mV = normalize_mV(gaussian_output, Emax_opt)
moving_average_normalized_mV = normalize_mV(moving_average_output, Emax_opt)


# spatial sensitivity for each normalized result
def compute_spatial_sensitivity(normalized_mV, Khalf_opt, n_opt):
    spatial_sensitivity = np.zeros_like(normalized_mV)
    non_one_indices = normalized_mV != 1
    spatial_sensitivity[non_one_indices] = Khalf_opt * (normalized_mV[non_one_indices] / (1 - normalized_mV[non_one_indices])) ** (1 / n_opt)
    spatial_sensitivity[~non_one_indices] = 1
    return spatial_sensitivity / np.max(spatial_sensitivity)

##bicubic_sensitivity = compute_spatial_sensitivity(bicubic_normalized_mV, Khalf_opt, n_opt)
gaussian_sensitivity = compute_spatial_sensitivity(gaussian_normalized_mV, Khalf_opt, n_opt)
moving_average_sensitivity = compute_spatial_sensitivity(moving_average_normalized_mV, Khalf_opt, n_opt)

# 2D Gaussian 
def gaussian_2d(xy, amp, xo, yo, sigma_x, sigma_y, theta, offset):
    (x, y) = xy
    x0 = float(xo)
    y0 = float(yo)
    a = (np.cos(theta)**2)/(2*sigma_x**2) + (np.sin(theta)**2)/(2*sigma_y**2)
    b = -(np.sin(2*theta))/(4*sigma_x**2) + (np.sin(2*theta))/(4*sigma_y**2)
    c = (np.sin(theta)**2)/(2*sigma_x**2) + (np.cos(theta)**2)/(2*sigma_y**2)
    g = offset + amp*np.exp( - (a*((x-x0)**2) + 2*b*(x-x0)*(y-y0) + c*((y-y0)**2)))
    return g.ravel()

# mesh grid
x = np.linspace(0, 20, 21)
y = np.linspace(0, 20, 21)
x, y = np.meshgrid(x, y)
xy = (x, y)

# initial guess for the parameters
initial_guess_gaussian = (1, 10, 10, 3, 3, 0, 0)
initial_guess_moving_avg = (1, 10, 10, 3, 3, 0, 0)

bounds = ([0, 0, 0, 0, 0, -np.inf, 0], [1, 20, 20, 10, 10, np.inf, 0.5]) # to bind the parameters for efficient optimization

# fit the Gaussian model to the data
popt_gaussian, _ = curve_fit(gaussian_2d, xy, gaussian_sensitivity.ravel(), p0=initial_guess_gaussian,bounds=bounds)
popt_moving_avg, _ = curve_fit(gaussian_2d, xy, moving_average_sensitivity.ravel(), p0=initial_guess_moving_avg,bounds=bounds)



def gaussian_value_at_mean(popt):
    amp, xo, yo, sigma_x, sigma_y, theta, offset = popt
    return offset + amp * np.exp(0)  # exp(0) is 1, since we evaluate at the mean (xo, yo)

# values for the fitted Gaussians at their means
value_at_mean_gaussian = gaussian_value_at_mean(popt_gaussian)
value_at_mean_moving_avg = gaussian_value_at_mean(popt_moving_avg)


# generate fitted data
fitted_gaussian = gaussian_2d((x, y), *popt_gaussian).reshape(21, 21)
fitted_moving_avg = gaussian_2d((x, y), *popt_moving_avg).reshape(21, 21)
# normalize by the value at the mean and subtract the offset and then divide by the amplitude to get the actual gaussian only
fitted_gaussian = ((fitted_gaussian / value_at_mean_gaussian) - popt_gaussian[6]) / popt_gaussian[0]
fitted_moving_avg = ((fitted_moving_avg / value_at_mean_moving_avg) - popt_moving_avg[6]) / popt_moving_avg[0]

# theta represents the angle of the major axis and the x-axis
def adjust_theta(popt):
    sigma_x, sigma_y = popt[3], popt[4]
    if sigma_x < sigma_y:
        popt[3], popt[4] = sigma_y, sigma_x  # swap sigma_x and sigma_y
        popt[5] += np.pi / 2  # adjust theta by 90 degrees
    popt[5] = np.mod(popt[5], np.pi)  # constrain theta to [0, π)
    return popt

popt_gaussian = adjust_theta(popt_gaussian)
popt_moving_avg = adjust_theta(popt_moving_avg)

print("\nGaussian Sensitivity - Optimized Parameters:")
print(f"Amplitude: {popt_gaussian[0]}")
print(f"x0: {popt_gaussian[1]}")
print(f"y0: {popt_gaussian[2]}")
print(f"sigma_x: {popt_gaussian[3]}")
print(f"sigma_y: {popt_gaussian[4]}")
print(f"theta: {popt_gaussian[5]}")
print(f"offset: {popt_gaussian[6]}")

print("\nMoving Average Sensitivity - Optimized Parameters:")
print(f"Amplitude: {popt_moving_avg[0]}")
print(f"x0: {popt_moving_avg[1]}")
print(f"y0: {popt_moving_avg[2]}")
print(f"sigma_x: {popt_moving_avg[3]}")
print(f"sigma_y: {popt_moving_avg[4]}")
print(f"theta: {popt_moving_avg[5]}")
print(f"offset: {popt_moving_avg[6]}")

plt.figure(figsize=(15, 10))

plt.subplot(2, 2, 1)
plt.imshow(gaussian_sensitivity, cmap='coolwarm', aspect='equal', vmin=0, vmax=1)
plt.title('Gaussian Filtering Sensitivity')
plt.colorbar(fraction=0.046, pad=0.04)

plt.subplot(2, 2, 2)
plt.imshow(fitted_gaussian, cmap='coolwarm', aspect='equal', vmin=0, vmax=1)
plt.title('Fitted Gaussian Filtering Sensitivity')
plt.colorbar(fraction=0.046, pad=0.04)

plt.subplot(2, 2, 3)
plt.imshow(moving_average_sensitivity, cmap='coolwarm', aspect='equal', vmin=0, vmax=1)
plt.title('Moving Average Sensitivity')
plt.colorbar(fraction=0.046, pad=0.04)

plt.subplot(2, 2, 4)
plt.imshow(fitted_moving_avg, cmap='coolwarm', aspect='equal', vmin=0, vmax=1)
plt.title('Fitted Moving Average Sensitivity')
plt.colorbar(fraction=0.046, pad=0.04)

plt.tight_layout()
plt.show()


