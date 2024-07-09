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
##moving_average_output = moving_average(output_values, window_size=3)

##bicubic_normalized_mV = normalize_mV(bicubic_output, Emax_opt)
gaussian_normalized_mV = normalize_mV(gaussian_output, Emax_opt)
##moving_average_normalized_mV = normalize_mV(moving_average_output, Emax_opt)


# spatial sensitivity for each normalized result
def compute_spatial_sensitivity(normalized_mV, Khalf_opt, n_opt):
    spatial_sensitivity = np.zeros_like(normalized_mV)
    non_one_indices = normalized_mV != 1
    spatial_sensitivity[non_one_indices] = Khalf_opt * (normalized_mV[non_one_indices] / (1 - normalized_mV[non_one_indices])) ** (1 / n_opt)
    spatial_sensitivity[~non_one_indices] = 1
    return spatial_sensitivity / np.max(spatial_sensitivity)

##bicubic_sensitivity = compute_spatial_sensitivity(bicubic_normalized_mV, Khalf_opt, n_opt)
gaussian_sensitivity = compute_spatial_sensitivity(gaussian_normalized_mV, Khalf_opt, n_opt)
##moving_average_sensitivity = compute_spatial_sensitivity(moving_average_normalized_mV, Khalf_opt, n_opt)

# 2D Gaussians (mixture and single)
def gaussian_2d(xy, xo, yo, sigma_x, sigma_y, theta, offset, xo2, yo2, sigma_x2, sigma_y2, theta2, weighting):
    (x, y) = xy
    x0 = float(xo)
    y0 = float(yo)
    x02 = float(xo2)
    y02 = float(yo2)
    amp = 1 - offset
    a = (np.cos(theta)**2)/(2*sigma_x**2) + (np.sin(theta)**2)/(2*sigma_y**2)
    b = -(np.sin(2*theta))/(4*sigma_x**2) + (np.sin(2*theta))/(4*sigma_y**2)
    c = (np.sin(theta)**2)/(2*sigma_x**2) + (np.cos(theta)**2)/(2*sigma_y**2)
    a2 = (np.cos(theta2)**2)/(2*sigma_x2**2) + (np.sin(theta2)**2)/(2*sigma_y2**2)
    b2 = -(np.sin(2*theta2))/(4*sigma_x2**2) + (np.sin(2*theta2))/(4*sigma_y2**2)
    c2 = (np.sin(theta2)**2)/(2*sigma_x2**2) + (np.cos(theta2)**2)/(2*sigma_y2**2)
    g = offset + amp*(np.exp( - (a*((x-xo)**2) + 2*b*(x-xo)*(y-yo) + c*((y-yo)**2)))*weighting + np.exp( - (a2*((x-xo2)**2) + 2*b2*(x-xo2)*(y-yo2) 
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

def random_initial_parameters(bounds, n_sets):
    params = []
    for _ in range(n_sets):
        param_set = []
        for lower, upper in zip(bounds[0], bounds[1]):
            if np.isfinite(lower) and np.isfinite(upper):  # if bounds are finite
                param_set.append(np.random.uniform(lower, upper))
            elif np.isfinite(lower):
                param_set.append(lower + abs(np.random.randn())) # if lower bound is finite
            elif np.isfinite(upper):
                param_set.append(upper - abs(np.random.randn())) # if upper bound is finite
            else: 
                param_set.append(np.random.randn())
        params.append(param_set)
    return params

# mesh grid
x = np.linspace(0, 20, 21)
y = np.linspace(0, 20, 21)
x, y = np.meshgrid(x, y)
xy = (x, y)

# to bind the parameters for efficient optimization: amp, xo, yo, sigma_x, sigma_y, theta, offset, xo2, yo2, sigma_x2, sigma_y2, theta2, weighting
bounds = ([0, 0, 1, 1, -np.inf, 0, 0, 0, 1, 1, -np.inf, 0], [20, 20, 10, 10, np.inf, 1, 20, 20, 10, 10, np.inf, 0.9])
bounds_single = ([0, 0, 1, 1, -np.inf, 0], [20, 20, 10, 10, np.inf, 1])
# generate 4 sets of random initial parameter values
random_params = random_initial_parameters(bounds, 12)
random_params_single = random_initial_parameters(bounds_single, 12)

best_popt = None
best_popt_single = None
lowest_ss_res = np.inf
lowest_ss_res_single = np.inf


# find the best parameter set
for initial_params in random_params:
    #print(initial_params)
    popt, _ = curve_fit(gaussian_2d, xy, gaussian_sensitivity.ravel(), p0=initial_params, bounds=bounds)
    fitted_gaussian = gaussian_2d((x, y), *popt).reshape(21, 21)
    residuals = gaussian_sensitivity - fitted_gaussian
    ss_res = np.sum(residuals**2)
    #print(ss_res)
    if ss_res < lowest_ss_res:
        lowest_ss_res = ss_res
        best_popt = popt
print(f"lowest_ss_res_mixture: {lowest_ss_res}")
print(f"best_popt_mixture: {best_popt}")

for initial_params in random_params_single:
    popt_single, _ = curve_fit(gaussian_2d_single, xy, gaussian_sensitivity.ravel(), p0=initial_params, bounds=bounds_single)
    fitted_gaussian_single = gaussian_2d_single((x, y), *popt_single).reshape(21, 21)
    residuals_single = gaussian_sensitivity - fitted_gaussian_single
    ss_res_single = np.sum(residuals_single**2)
    
    if ss_res_single < lowest_ss_res_single:
        lowest_ss_res_single = ss_res_single
        best_popt_single = popt_single
print(f"lowest_ss_res_single: {lowest_ss_res_single}")
print(f"best_popt_single: {best_popt_single}")

def calculate_likelihood(residuals):
    return np.sum(norm.logpdf(residuals, loc = 0, scale = std(residuals)))

# likelihood for the best parameters of both models
likelihood_mixture = calculate_likelihood(gaussian_sensitivity - gaussian_2d((x, y), *best_popt).reshape(21, 21))
print(likelihood_mixture)
likelihood_single = calculate_likelihood(gaussian_sensitivity - gaussian_2d_single((x, y), *best_popt_single).reshape(21, 21))
print(likelihood_single)

# likelihood ratio
likelihood_ratio = likelihood_mixture - likelihood_single

print(f"Likelihood Ratio: {likelihood_ratio}")

##p = chi2.cdf(likelihood_ratio, 4) # L2 has 4 DoF more than L1
##
##print('p: %.5f' % p)

# determine which model has the higher likelihood ratio
if likelihood_ratio > 4:
    
    print("Mixture model is favored.")
 
    weighting_gaussian = best_popt[11]

    def gaussian_value_at_means(popt):
        xo, yo, sigma_x, sigma_y, theta, offset, xo2, yo2, sigma_x2, sigma_y2, theta2, weighting = popt
        amp = 1 - offset
        a = (np.cos(theta)**2)/(2*sigma_x**2) + (np.sin(theta)**2)/(2*sigma_y**2)
        b = -(np.sin(2*theta))/(4*sigma_x**2) + (np.sin(2*theta))/(4*sigma_y**2)
        c = (np.sin(theta)**2)/(2*sigma_x**2) + (np.cos(theta)**2)/(2*sigma_y**2)
        a2 = (np.cos(theta2)**2)/(2*sigma_x2**2) + (np.sin(theta2)**2)/(2*sigma_y2**2)
        b2 = -(np.sin(2*theta2))/(4*sigma_x2**2) + (np.sin(2*theta2))/(4*sigma_y2**2)
        c2 = (np.sin(theta2)**2)/(2*sigma_x2**2) + (np.cos(theta2)**2)/(2*sigma_y2**2)
        value_at_mean1 = offset + amp * (np.exp(0)*weighting + np.exp( - (a2*((xo-xo2)**2) + 2*b2*(xo-xo2)*(yo-yo2) + c2*((yo-yo2)**2)))*(1- weighting))
        value_at_mean2 = offset + amp * (np.exp( - (a*((xo2-xo)**2) + 2*b*(xo2-xo)*(yo2-yo) + c*((yo2-yo)**2)))*weighting + np.exp(0)*(1-weighting))
        return value_at_mean1, value_at_mean2  # exp(0) is 1, since we evaluate at the mean (xo, yo)

    # values for the fitted Gaussians at their means
    value_at_mean1_gaussian = gaussian_value_at_means(best_popt)[0]
    value_at_mean2_gaussian = gaussian_value_at_means(best_popt)[1]

    # normalize by the value at the 'primary' mean (the one cell we are recording from) and subtract the offset and then divide by the amplitude to get the actual gaussian only
    if weighting_gaussian >= 0.5:
        fitted_gaussian = ((fitted_gaussian / value_at_mean1_gaussian) - best_popt[5]) / (1 - best_popt[5])
    else:
        fitted_gaussian = ((fitted_gaussian / value_at_mean2_gaussian) - best_popt[5]) / (1 - best_popt[5])

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

    best_popt = adjust_theta(best_popt)
    amp_gaussian = 1 - best_popt[5]

    residuals = gaussian_sensitivity - fitted_gaussian
    ss_res = np.sum(residuals**2)

    print("\nGaussian Sensitivity - Optimized Parameters:")
    print(f"Amplitude: {amp_gaussian}")
    print(f"x0: {best_popt[0]}")
    print(f"y0: {best_popt[1]}")
    print(f"sigma_x: {best_popt[2]}")
    print(f"sigma_y: {best_popt[3]}")
    print(f"theta: {best_popt[4]}")
    print(f"offset: {best_popt[5]}")
    print(f"x02: {best_popt[6]}")
    print(f"y02: {best_popt[7]}")
    print(f"sigma_x2: {best_popt[8]}")
    print(f"sigma_y2: {best_popt[9]}")
    print(f"theta2: {best_popt[10]}")
    print(f"weighting: {best_popt[11]}")

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
else:
    print("Single-mean model is favored.")

    def gaussian_value_at_mean(popt):
        xo, yo, sigma_x, sigma_y, theta, offset = popt
        amp = 1 - offset
        return offset + amp * np.exp(0)  # exp(0) is 1, since we evaluate at the mean (xo, yo)
    
    value_at_mean_gaussian = gaussian_value_at_mean(best_popt_single)
    fitted_gaussian_single = ((fitted_gaussian_single / value_at_mean_gaussian) - best_popt_single[5]) / (1 - best_popt_single[5])

    # theta represents the angle of the major axis and the x-axis
    def adjust_theta(popt):
        sigma_x, sigma_y = popt[2], popt[3]
        if sigma_x < sigma_y:
            popt[2], popt[3] = sigma_y, sigma_x  # swap sigma_x and sigma_y
            popt[4] += np.pi / 2  # adjust theta by 90 degrees
        popt[4] = np.mod(popt[4], np.pi)  # constrain theta to [0, π)
        return popt

    best_popt_single = adjust_theta(best_popt_single)
    amp_gaussian = 1 - best_popt_single[5]
    
    print("\nGaussian Sensitivity - Optimized Parameters:")
    print(f"Amplitude: {amp_gaussian}")
    print(f"x0: {best_popt_single[0]}")
    print(f"y0: {best_popt_single[1]}")
    print(f"sigma_x: {best_popt_single[2]}")
    print(f"sigma_y: {best_popt_single[3]}")
    print(f"theta: {best_popt_single[4]}")
    print(f"offset: {best_popt_single[5]}")

    plt.figure(figsize=(15, 10))

    plt.subplot(1, 2, 1)
    plt.imshow(gaussian_sensitivity, cmap='coolwarm', aspect='equal', vmin=0, vmax=1)
    plt.title('Gaussian Filtering Sensitivity')
    plt.colorbar(fraction=0.046, pad=0.04)

    plt.subplot(1, 2, 2)
    plt.imshow(fitted_gaussian_single, cmap='coolwarm', aspect='equal', vmin=0, vmax=1)
    plt.title('Fitted Gaussian Filtering Sensitivity')
    plt.colorbar(fraction=0.046, pad=0.04)

    plt.tight_layout()
    plt.show()
