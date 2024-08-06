import numpy as np
import scipy.ndimage
import matplotlib.pyplot as plt
from scipy.interpolate import interp2d
import sys
import subprocess
from scipy.optimize import curve_fit
from scipy.interpolate import griddata
from scipy.stats import norm
from numpy import std
from scipy.stats.distributions import chi2
from scipy.ndimage import label, center_of_mass

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

gaussian_output = gaussian_filtering(output_values, sigma=1)

gaussian_normalized_mV = normalize_mV(gaussian_output, Emax_opt)


# spatial sensitivity for each normalized result
def compute_spatial_sensitivity(normalized_mV, Khalf_opt, n_opt):
    spatial_sensitivity = np.zeros_like(normalized_mV)
    non_one_indices = normalized_mV != 1
    spatial_sensitivity[non_one_indices] = Khalf_opt * (normalized_mV[non_one_indices] / (1 - normalized_mV[non_one_indices])) ** (1 / n_opt)
    spatial_sensitivity[~non_one_indices] = 1
    return spatial_sensitivity / np.max(spatial_sensitivity)

gaussian_sensitivity = compute_spatial_sensitivity(gaussian_normalized_mV, Khalf_opt, n_opt)

fig, ax = plt.subplots()

# contour plot 
contour = ax.contour(gaussian_sensitivity, levels=[float(sys.argv[3])], colors='red')

# binary mask where gaussian_sensitivity > n
binary_mask = gaussian_sensitivity > float(sys.argv[3])

# label connected regions 
labeled_array, num_features = label(binary_mask) # num_features is the number of shapes

# centroids
centroids = center_of_mass(binary_mask, labeled_array, range(1, num_features + 1))

for idx, centroid in enumerate(centroids, start=1):
    print(f"Centroid of region {idx}: {centroid}") ## (y,x)

for centroid in centroids:
    x = centroid[1] # centroid x-coordinate (column index)
    y = centroid[0] # centroid y-coordinate (row index)
    ax.plot(x, y, 'bo') 
print(centroid[1])
plt.xlabel('X')
plt.ylabel('Y')
plt.title('Contour Map with Centroids')
plt.gca().invert_yaxis()  # invert the y-axis to match sensitivity indexing

plt.figure(figsize=(15, 10))

plt.show()



# 2D Gaussians (mixture and single)
def gaussian_2d(xy, xo, yo, sigma_x, sigma_y, theta, amp, xo2, yo2, sigma_x2, sigma_y2, theta2, weighting):
    (x, y) = xy
    x0 = float(xo)
    y0 = float(yo)
    x02 = float(xo2)
    y02 = float(yo2)
    a = (np.cos(theta)**2)/(2*sigma_x**2) + (np.sin(theta)**2)/(2*sigma_y**2)
    b = -(np.sin(2*theta))/(4*sigma_x**2) + (np.sin(2*theta))/(4*sigma_y**2)
    c = (np.sin(theta)**2)/(2*sigma_x**2) + (np.cos(theta)**2)/(2*sigma_y**2)
    a2 = (np.cos(theta2)**2)/(2*sigma_x2**2) + (np.sin(theta2)**2)/(2*sigma_y2**2)
    b2 = -(np.sin(2*theta2))/(4*sigma_x2**2) + (np.sin(2*theta2))/(4*sigma_y2**2)
    c2 = (np.sin(theta2)**2)/(2*sigma_x2**2) + (np.cos(theta2)**2)/(2*sigma_y2**2)
    g = amp*(np.exp( - (a*((x-xo)**2) + 2*b*(x-xo)*(y-yo) + c*((y-yo)**2)))*weighting + np.exp( - (a2*((x-xo2)**2) + 2*b2*(x-xo2)*(y-yo2) 
                            + c2*((y-yo2)**2)))*(1- weighting)) # weighting should be between 0 and 1 so that the area under the two distributions adds to 1.0
    return g.ravel()

def gaussian_2d_single(xy, xo, yo, sigma_x, sigma_y, theta, offset):
    (x, y) = xy
    x0 = float(xo)
    y0 = float(yo)
    amp = 1 - offset
    a = (np.cos(theta)**2)/(2*sigma_x**2) + (np.sin(theta)**2)/(2*sigma_y**2)
    b = -(np.sin(2*theta))/(4*sigma_x**2) + (np.sin(2*theta))/(4*sigma_y**2)
    c = (np.sin(theta)**2)/(2*sigma_x**2) + (np.cos(theta)**2)/(2*sigma_y**2)
    g = offset + amp*np.exp(- (a*((x-x0)**2) + 2*b*(x-x0)*(y-y0) + c*((y-y0)**2)))
    return g.ravel()

# mesh grid
x = np.linspace(0, 20, 21)
y = np.linspace(0, 20, 21)
x, y = np.meshgrid(x, y)
xy = (x, y)

# to bind the parameters for efficient optimization: amp, xo, yo, sigma_x, sigma_y, theta, offset, xo2, yo2, sigma_x2, sigma_y2, theta2, weighting
bounds = ([0, 0, 1, 1, -np.inf, 0, 0, 0, 1, 1, -np.inf, 0], [20, 20, 10, 10, np.inf, 1, 20, 20, 10, 10, np.inf, 0.9])
bounds_single = ([0, 0, 1, 1, -np.inf, 0], [20, 20, 10, 10, np.inf, 1])

initial_params_single = (centroid[1], centroid[0], 2, 2, 0, 0)

if len(centroids) == 1:
    popt, _ = curve_fit(gaussian_2d_single, xy, gaussian_sensitivity.ravel(), p0=initial_params_single, bounds=bounds_single)
    fitted_gaussian = gaussian_2d_single((x, y), *popt).reshape(21, 21)
    def gaussian_value_at_mean(popt):
        xo, yo, sigma_x, sigma_y, theta, offset = popt
        amp = 1 - offset
        return offset + amp * np.exp(0)  # exp(0) is 1, since we evaluate at the mean (xo, yo)
    
    value_at_mean_gaussian = gaussian_value_at_mean(popt)
    fitted_gaussian = ((fitted_gaussian / value_at_mean_gaussian) - popt[5]) / (1 - popt[5])

    # theta represents the angle of the major axis and the x-axis
    def adjust_theta(popt):
        sigma_x, sigma_y = popt[2], popt[3]
        if sigma_x < sigma_y:
            popt[2], popt[3] = sigma_y, sigma_x  # swap sigma_x and sigma_y
            popt[4] += np.pi / 2  # adjust theta by 90 degrees
        popt[4] = np.mod(popt[4], np.pi)  # constrain theta to [0, π)
        return popt

    popt = adjust_theta(popt)
    amp_gaussian = 1 - popt[5]
    
    print("\nGaussian Sensitivity - Optimized Parameters:")
    print(f"Amplitude: {amp_gaussian}")
    print(f"x0: {popt[0]}")
    print(f"y0: {popt[1]}")
    print(f"sigma_x: {popt[2]}")
    print(f"sigma_y: {popt[3]}")
    print(f"theta: {popt[4]}")
    print(f"offset: {popt[5]}")

    plt.figure(figsize=(15, 10))

    plt.subplot(1, 2, 1)
    plt.imshow(gaussian_sensitivity, cmap='coolwarm', aspect='equal', vmin=0, vmax=1)
    plt.title('Gaussian Filtering Sensitivity')
    plt.colorbar(fraction=0.046, pad=0.04)

    plt.subplot(1, 2, 2)
    plt.imshow(fitted_gaussian, cmap='coolwarm', aspect='equal', vmin=0, vmax=1)
    plt.title('Fitted Gaussian Filtering Sensitivity')
    plt.colorbar(fraction=0.046, pad=0.04)

    plt.tight_layout()
    plt.show()

if len(centroids) == 2:
    initial_params = (centroids[0][1], centroids[0][0], 2, 2, 0, 0, centroids[1][1], centroids[1][0], 2, 2, 0, 0.5)
    popt, _ = curve_fit(gaussian_2d, xy, gaussian_sensitivity.ravel(), p0=initial_params, bounds=bounds)
    fitted_gaussian = gaussian_2d((x, y), *popt).reshape(21, 21)
    weighting_gaussian = popt[11]
    
    def gaussian_value_at_means(popt):
        xo, yo, sigma_x, sigma_y, theta, amp, xo2, yo2, sigma_x2, sigma_y2, theta2, weighting = popt
        a = (np.cos(theta)**2)/(2*sigma_x**2) + (np.sin(theta)**2)/(2*sigma_y**2)
        b = -(np.sin(2*theta))/(4*sigma_x**2) + (np.sin(2*theta))/(4*sigma_y**2)
        c = (np.sin(theta)**2)/(2*sigma_x**2) + (np.cos(theta)**2)/(2*sigma_y**2)
        a2 = (np.cos(theta2)**2)/(2*sigma_x2**2) + (np.sin(theta2)**2)/(2*sigma_y2**2)
        b2 = -(np.sin(2*theta2))/(4*sigma_x2**2) + (np.sin(2*theta2))/(4*sigma_y2**2)
        c2 = (np.sin(theta2)**2)/(2*sigma_x2**2) + (np.cos(theta2)**2)/(2*sigma_y2**2)
        value_at_mean1 = amp * (np.exp(0)*weighting + np.exp( - (a2*((xo-xo2)**2) + 2*b2*(xo-xo2)*(yo-yo2) + c2*((yo-yo2)**2)))*(1- weighting))
        value_at_mean2 = amp * (np.exp( - (a*((xo2-xo)**2) + 2*b*(xo2-xo)*(yo2-yo) + c*((yo2-yo)**2)))*weighting + np.exp(0)*(1-weighting))
        return value_at_mean1, value_at_mean2  # exp(0) is 1, since we evaluate at the mean (xo, yo)

    # values for the fitted Gaussians at their means
    value_at_mean1_gaussian = gaussian_value_at_means(popt)[0]
    value_at_mean2_gaussian = gaussian_value_at_means(popt)[1]

    # normalize by the value at the 'primary' mean (the one cell we are recording from) and subtract the offset and then divide by the amplitude to get the actual gaussian only
    if weighting_gaussian >= 0.5:
        fitted_gaussian = (fitted_gaussian / value_at_mean1_gaussian) / popt[5]
    else:
        fitted_gaussian = (fitted_gaussian / value_at_mean2_gaussian) / popt[5]

    # theta represents the angle of the major axis and the x-axis for the two gaussians
    def adjust_theta(popt):
        sigma_x, sigma_y, sigma_x2, sigma_y2 = popt[2], popt[3], popt[8], popt[9]
        if sigma_x < sigma_y:
            popt[2], popt[3] = sigma_y, sigma_x  # swap sigma_x and sigma_y
            popt[4] += np.pi / 2  # adjust theta by 90 degrees
        popt[4] = np.mod(popt[4], np.pi)  # constrain theta to [0, π)
        if sigma_x2 < sigma_y2:
            popt[8], popt[9] = sigma_y2, sigma_x2  # swap sigma_x2 and sigma_y2
            popt[10] += np.pi / 2  # adjust theta2 by 90 degrees
        popt[10] = np.mod(popt[10], np.pi)  # constrain theta2 to [0, π)
        return popt

    popt = adjust_theta(popt)
    amp_gaussian = popt[5]

    print("\nGaussian Sensitivity - Optimized Parameters:")
    print(f"Amplitude: {popt[5]}")
    print(f"x0: {popt[0]}")
    print(f"y0: {popt[1]}")
    print(f"sigma_x: {popt[2]}")
    print(f"sigma_y: {popt[3]}")
    print(f"theta: {popt[4]}")
    print(f"x02: {popt[6]}")
    print(f"y02: {popt[7]}")
    print(f"sigma_x2: {popt[8]}")
    print(f"sigma_y2: {popt[9]}")
    print(f"theta2: {popt[10]}")
    print(f"weighting: {popt[11]}")

    plt.figure(figsize=(15, 10))

    plt.subplot(1, 2, 1)
    plt.imshow(gaussian_sensitivity, cmap='coolwarm', aspect='equal', vmin=0, vmax=1)
    plt.title('Gaussian Filtering Sensitivity')
    plt.colorbar(fraction=0.046, pad=0.04)

    plt.subplot(1, 2, 2)
    plt.imshow(fitted_gaussian, cmap='coolwarm', aspect='equal', vmin=0, vmax=1)
    plt.title('Fitted Gaussian Filtering Sensitivity')
    plt.colorbar(fraction=0.046, pad=0.04)

    plt.tight_layout()
    plt.show()


