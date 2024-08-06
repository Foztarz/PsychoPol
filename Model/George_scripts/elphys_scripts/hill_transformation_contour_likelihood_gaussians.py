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
from matplotlib.patches import Ellipse
from matplotlib.lines import Line2D
from scipy.optimize import minimize
from skimage.draw import line
from scipy.ndimage import map_coordinates
from scipy.interpolate import RegularGridInterpolator

# Hill equation
def hill_equation(I, Emax, Khalf, n):
    return Emax * (I**n) / (I**n + Khalf**n)

data = np.loadtxt(sys.argv[1])  

# intensity and response values
intensity_log = data[:, 1]
response_values = data[:, 0]

# this for taking only response values until saturation
max_response = np.max(response_values)
threshold = 0.95 * max_response
max_response_index = np.argmax(response_values)
below_threshold_index = None
for i in range(max_response_index + 1, len(response_values)):
    if intensity_log[i] < threshold:
        below_threshold_index = i
        break
if below_threshold_index is not None:
    response_values = response_values[:below_threshold_index] # until 1st point after max below 95% of maximum response
    response_values = np.append(response_values, [np.max(response_values)]) # duplicate the max response at the end of the curve
    # intensity values from log scale to linear scale
    intensity_log = data[:, 1][:below_threshold_index+1] # take only values before saturation
    intensity_linear = 10**intensity_log[:below_threshold_index+1] # take only values before saturation    
else:
    response_values = response_values
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
print('max value of raw spatial sensitivity data (mV): ', np.max(output_values))

# plot raw ss data
plt.figure(figsize=(10, 10))

plt.imshow(output_values, cmap='coolwarm', aspect='equal')
plt.title('Raw mV spatial sensitivity')
plt.colorbar(fraction=0.046, pad=0.04)
plt.show()

output_values = output_values/np.max(output_values) # normalize the ss data

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

# not used currently
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


gaussian_normalized_mV = gaussian_filtering(output_values, sigma=1)

print('ratio max(smoothed_values)/Emax_opt:', np.max(gaussian_normalized_mV) / Emax_opt)

#gaussian_normalized_mV = normalize_mV(gaussian_output, Emax_opt)

# spatial sensitivity for each normalized result
def compute_spatial_sensitivity(normalized_mV, Khalf_opt, n_opt):
    spatial_sensitivity = np.zeros_like(normalized_mV)
    non_one_indices = normalized_mV != 1
    spatial_sensitivity[non_one_indices] = Khalf_opt * (normalized_mV[non_one_indices] / (1 - normalized_mV[non_one_indices])) ** (1 / n_opt)
    spatial_sensitivity[~non_one_indices] = 1
    return spatial_sensitivity / np.max(spatial_sensitivity)

gaussian_sensitivity = compute_spatial_sensitivity(gaussian_normalized_mV, Khalf_opt, n_opt)

with open(sys.argv[4], 'a') as file:
    for row in gaussian_sensitivity:
        for value in row:
            file.write(f'{value}\n')
        
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

plt.xlabel('X')
plt.ylabel('Y')
plt.title('Contour Map with Centroids')
plt.gca().invert_yaxis()  # invert the y-axis to match sensitivity indexing

plt.show()

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

def gaussian_2d_triple(xy, xo, yo, sigma_x, sigma_y, theta, offset, xo2, yo2, sigma_x2, sigma_y2, theta2, weighting, xo3, yo3, sigma_x3, sigma_y3, theta3, weighting2):
    (x, y) = xy
    x0 = float(xo)
    y0 = float(yo)
    x02 = float(xo2)
    y02 = float(yo2)
    x03 = float(xo3)
    y03 = float(yo3)
    amp = 1 - offset
    a = (np.cos(theta)**2)/(2*sigma_x**2) + (np.sin(theta)**2)/(2*sigma_y**2)
    b = -(np.sin(2*theta))/(4*sigma_x**2) + (np.sin(2*theta))/(4*sigma_y**2)
    c = (np.sin(theta)**2)/(2*sigma_x**2) + (np.cos(theta)**2)/(2*sigma_y**2)
    a2 = (np.cos(theta2)**2)/(2*sigma_x2**2) + (np.sin(theta2)**2)/(2*sigma_y2**2)
    b2 = -(np.sin(2*theta2))/(4*sigma_x2**2) + (np.sin(2*theta2))/(4*sigma_y2**2)
    c2 = (np.sin(theta2)**2)/(2*sigma_x2**2) + (np.cos(theta2)**2)/(2*sigma_y2**2)
    a3 = (np.cos(theta3)**2)/(2*sigma_x3**2) + (np.sin(theta3)**2)/(2*sigma_y3**2)
    b3 = -(np.sin(2*theta3))/(4*sigma_x3**2) + (np.sin(2*theta3))/(4*sigma_y3**2)
    c3 = (np.sin(theta3)**2)/(2*sigma_x3**2) + (np.cos(theta3)**2)/(2*sigma_y3**2)
    g = offset + amp*(np.exp( - (a*((x-xo)**2) + 2*b*(x-xo)*(y-yo) + c*((y-yo)**2)))*weighting + np.exp( - (a2*((x-xo2)**2) + 2*b2*(x-xo2)*(y-yo2) 
                            + c2*((y-yo2)**2)))*weighting2*(1- weighting) + np.exp( - (a3*((x-xo3)**2) + 2*b3*(x-xo3)*(y-yo3) 
                            + c3*((y-yo3)**2)))*(1 - weighting2)*(1- weighting)) # weighting should be between 0 and 1 so that the area under the two distributions adds to 1.0
    return g.ravel()

def negative_log_likelihood_single(params, xy, data, centroids):
    xo, yo, sigma_x, sigma_y, theta, offset = params
    model = gaussian_2d_single(xy, xo, yo, sigma_x, sigma_y, theta, offset)
    model = model.reshape(data.shape)
    residuals = data - model
    prior_xo = norm.logpdf(xo, loc=centroids[0][1], scale=0.1)
    prior_yo = norm.logpdf(yo, loc=centroids[0][0], scale=0.1)
    prior_sigma_x = norm.logpdf(sigma_x, loc=4/2.355, scale=0.25)
    prior_sigma_y = norm.logpdf(sigma_y, loc=4/2.355, scale=0.25)
    nll = - np.sum(norm.logpdf(residuals, loc = 0, scale = std(residuals))) - (prior_xo + prior_yo) - (prior_sigma_x+prior_sigma_y)
    return nll

def negative_log_likelihood_double(params, xy, data, centroids):
    xo, yo, sigma_x, sigma_y, theta, offset, xo2, yo2, sigma_x2, sigma_y2, theta2, weighting = params
    model = gaussian_2d(xy, xo, yo, sigma_x, sigma_y, theta, offset, xo2, yo2, sigma_x2, sigma_y2, theta2, weighting)
    model = model.reshape(data.shape)
    residuals = data - model
    prior_xo = norm.logpdf(xo, loc=centroids[0][1], scale=0.1)
    prior_yo = norm.logpdf(yo, loc=centroids[0][0], scale=0.1)
    prior_xo2 = norm.logpdf(xo2, loc=centroids[1][1], scale=0.1)
    prior_yo2 = norm.logpdf(yo2, loc=centroids[1][0], scale=0.1)
    prior_sigma_x = norm.logpdf(sigma_x, loc=4/2.355, scale=0.25)
    prior_sigma_y = norm.logpdf(sigma_y, loc=4/2.355, scale=0.25)
    prior_sigma_x2 = norm.logpdf(sigma_x2, loc=4/2.355, scale=0.25)
    prior_sigma_y2 = norm.logpdf(sigma_y2, loc=4/2.355, scale=0.25)
    nll = - np.sum(norm.logpdf(residuals, loc = 0, scale = std(residuals))) - (prior_xo + prior_yo) - (prior_xo2 + prior_yo2) - (prior_sigma_x+prior_sigma_y+prior_sigma_x2+prior_sigma_y2)
    return nll

def negative_log_likelihood_triple(params, xy, data, centroids):
    xo, yo, sigma_x, sigma_y, theta, offset, xo2, yo2, sigma_x2, sigma_y2, theta2, weighting, xo3, yo3, sigma_x3, sigma_y3, theta3, weighting2 = params
    model = gaussian_2d_triple(xy, xo, yo, sigma_x, sigma_y, theta, offset, xo2, yo2, sigma_x2, sigma_y2, theta2, weighting, xo3, yo3, sigma_x3, sigma_y3, theta3, weighting2)
    model = model.reshape(data.shape)
    residuals = data - model
    prior_xo = norm.logpdf(xo, loc=centroids[0][1], scale=0.1)
    prior_yo = norm.logpdf(yo, loc=centroids[0][0], scale=0.1)
    prior_xo2 = norm.logpdf(xo2, loc=centroids[1][1], scale=0.1)
    prior_yo2 = norm.logpdf(yo2, loc=centroids[1][0], scale=0.1)
    prior_xo3 = norm.logpdf(xo3, loc=centroids[2][1], scale=0.1)
    prior_yo3 = norm.logpdf(yo3, loc=centroids[2][0], scale=0.1)
    prior_sigma_x = norm.logpdf(sigma_x, loc=4/2.355, scale=0.25)
    prior_sigma_y = norm.logpdf(sigma_y, loc=4/2.355, scale=0.25)
    prior_sigma_x2 = norm.logpdf(sigma_x2, loc=4/2.355, scale=0.25)
    prior_sigma_y2 = norm.logpdf(sigma_y2, loc=4/2.355, scale=0.25)
    prior_sigma_x3 = norm.logpdf(sigma_x3, loc=4/2.355, scale=0.25)
    prior_sigma_y3 = norm.logpdf(sigma_y3, loc=4/2.355, scale=0.25)
    
    nll = - np.sum(norm.logpdf(residuals, loc = 0, scale = std(residuals))) - (prior_xo + prior_yo) - (prior_xo2 + prior_yo2) - (prior_xo3 + prior_yo3) - (prior_sigma_x+prior_sigma_y+prior_sigma_x2+prior_sigma_y2+prior_sigma_x3+prior_sigma_y3)
    return nll
    
# mesh grid
x = np.linspace(0, 20, 21)
y = np.linspace(0, 20, 21)
x, y = np.meshgrid(x, y)
xy = (x, y)

# to bind the parameters for efficient optimization: amp, xo, yo, sigma_x, sigma_y, theta, offset, xo2, yo2, sigma_x2, sigma_y2, theta2, weighting
bounds = [(0, 20), (0, 20), (0.1, 5), (0.1, 5), (-np.inf, np.inf), (0, 1), (0, 20), (0, 20), (0.1, 5), (0.1, 5), (-np.inf, np.inf), (0, 0.9)]
bounds_single = [(0, 20), (0, 20), (0.1, 5), (0.1, 5), (-np.inf, np.inf), (0, 1)]
bounds_triple = [(0, 20), (0, 20), (0.1, 5), (0.1, 5), (-np.inf, np.inf), (0, 1), (0, 20), (0, 20), (0.1, 5), (0.1, 5), (-np.inf, np.inf), (0, 0.9), (0, 20), (0, 20), (0.1, 5), (0.1, 5), (-np.inf, np.inf), (0, 0.9)]


initial_params_single = (centroid[1], centroid[0], 1, 1, 0, 0)

if len(centroids) == 1:
    result = minimize(negative_log_likelihood_single, initial_params_single, args=(xy, gaussian_sensitivity, centroids), bounds = bounds_single, method = 'L-BFGS-B') # , options={"disp": True})
    popt = result.x
    
    # define a higher resolution for the gaussian fit
    x = np.linspace(0, 20, 100)
    y = np.linspace(0, 20, 100)
    x, y = np.meshgrid(x, y)
    
    fitted_gaussian = gaussian_2d_single((x, y), *popt).reshape(100, 100)
    
    # Function to compute Gaussian value at the mean
    def gaussian_value_at_mean(popt):
        xo, yo, sigma_x, sigma_y, theta, offset = popt
        amp = 1 - offset
        return offset + amp * np.exp(0)  # exp(0) is 1, since we evaluate at the mean (xo, yo)
    
    value_at_mean_gaussian = gaussian_value_at_mean(popt)
    fitted_gaussian = ((fitted_gaussian / value_at_mean_gaussian) - popt[5]) / (1 - popt[5])

    # Function to adjust theta and sigma values
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
    
    # parameters for the ellipse
    x0, y0 = popt[0], popt[1]
    sigma_x, sigma_y = popt[2], popt[3]
    theta = popt[4]
    major_axis = max(sigma_x, sigma_y) * 2.355
    minor_axis = min(sigma_x, sigma_y) * 2.355
    
    # ellipse
    ellipse = Ellipse((x0, y0), width=major_axis, height=minor_axis, angle=-np.degrees(theta), edgecolor='r', fc='None', lw=2)
    plt.gca().add_patch(ellipse)
    
    # endpoints of the lines representing sigma_x and sigma_y
    cos_theta = np.cos(-theta)
    sin_theta = np.sin(-theta)
    
    # major axis line
    x_major_end = x0 + (major_axis / 2) * cos_theta
    y_major_end = y0 + (major_axis / 2) * sin_theta
    x_minor_end = x0 - (major_axis / 2) * cos_theta
    y_minor_end = y0 - (major_axis / 2) * sin_theta
    
    # minor axis line
    x_minor_end_1 = x0 + (minor_axis / 2) * -sin_theta
    y_minor_end_1 = y0 + (minor_axis / 2) * cos_theta
    x_minor_end_2 = x0 - (minor_axis / 2) * -sin_theta
    y_minor_end_2 = y0 - (minor_axis / 2) * cos_theta
    
    # lines
    line_major = Line2D([x0, x_major_end], [y0, y_major_end], color='blue', linewidth=2)
    plt.gca().add_line(line_major)
    line_major = Line2D([x0, x_minor_end], [y0, y_minor_end], color='blue', linewidth=2)
    plt.gca().add_line(line_major)
    
    line_minor = Line2D([x0, x_minor_end_1], [y0, y_minor_end_1], color='green', linewidth=2)
    plt.gca().add_line(line_minor)
    line_minor = Line2D([x0, x_minor_end_2], [y0, y_minor_end_2], color='green', linewidth=2)
    plt.gca().add_line(line_minor)

    # annotate the sigmas
    plt.text(x_major_end, y_major_end, f'$\sigma_x={sigma_x:.2f}$', color='blue', fontsize=12, ha='center', va='center')
    plt.text(x_minor_end_1, y_minor_end_1, f'$\sigma_y={sigma_y:.2f}$', color='green', fontsize=12, ha='center', va='center')
    
    plt.subplot(1, 2, 2)
    plt.imshow(fitted_gaussian, cmap='coolwarm', aspect='equal', vmin=0, vmax=1)
    plt.title('Fitted Gaussian Filtering Sensitivity')
    plt.colorbar(fraction=0.046, pad=0.04)

    plt.tight_layout()
    plt.show()

    # calculate circle radius of main RF
    ellipse_surface = np.pi * (major_axis/2) * (minor_axis/2)
    circle_radius = np.sqrt(ellipse_surface/np.pi)
    print(f'circle radius of main RF: {circle_radius} degrees')
     
if len(centroids) == 2:
    initial_params = (centroids[0][1], centroids[0][0], 1, 1, 0, 0, centroids[1][1], centroids[1][0], 1, 1, 0, 0.5)
    result = minimize(negative_log_likelihood_double, initial_params, args=(xy, gaussian_sensitivity, centroids), bounds = bounds, method = 'L-BFGS-B') # , options={"disp": True})
    popt = result.x
    
    # define a higher resolution for the gaussian fit
    x = np.linspace(0, 20, 100)
    y = np.linspace(0, 20, 100)
    x, y = np.meshgrid(x, y)
    
    fitted_gaussian = gaussian_2d((x, y), *popt).reshape(100,100)
    weighting_gaussian = popt[11]
    
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
    value_at_mean1_gaussian = gaussian_value_at_means(popt)[0]
    value_at_mean2_gaussian = gaussian_value_at_means(popt)[1]

    # normalize by the value at the 'primary' mean (the one cell we are recording from) and subtract the offset and then divide by the amplitude to get the actual gaussian only
    if weighting_gaussian >= 0.5:
        fitted_gaussian = ((fitted_gaussian / value_at_mean1_gaussian) - popt[5]) / (1 - popt[5])
    else:
        fitted_gaussian = ((fitted_gaussian / value_at_mean2_gaussian) - popt[5]) / (1 - popt[5])

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
    amp_gaussian = 1 - popt[5]

    print("\nGaussian Sensitivity - Optimized Parameters:")
    print(f"Amplitude: {amp_gaussian}")
    print(f"x0: {popt[0]}")
    print(f"y0: {popt[1]}")
    print(f"sigma_x: {popt[2]}")
    print(f"sigma_y: {popt[3]}")
    print(f"theta: {popt[4]}")
    print(f"offset: {popt[5]}")
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

    # parameters for the ellipses
    x0, y0, x02, y02 = popt[0], popt[1], popt[6], popt[7]
    sigma_x, sigma_y, sigma_x2, sigma_y2 = popt[2], popt[3], popt[8], popt[9]
    theta, theta2 = popt[4], popt[10]
    major_axis = max(sigma_x, sigma_y) * 2.355
    minor_axis = min(sigma_x, sigma_y) * 2.355
    major_axis2 = max(sigma_x2, sigma_y2) * 2.355
    minor_axis2 = min(sigma_x2, sigma_y2) * 2.355
    
    # ellipses
    ellipse = Ellipse((x0, y0), width=major_axis, height=minor_axis, angle=-np.degrees(theta), edgecolor='r', fc='None', lw=2)
    ellipse2 = Ellipse((x02, y02), width=major_axis2, height=minor_axis2, angle=-np.degrees(theta2), edgecolor='r', fc='None', lw=2)
    plt.gca().add_patch(ellipse)
    plt.gca().add_patch(ellipse2)
    
    # endpoints of the lines representing sigma_x and sigma_y
    cos_theta = np.cos(-theta)
    sin_theta = np.sin(-theta)
    cos_theta2 = np.cos(-theta2)
    sin_theta2 = np.sin(-theta2)
    
    # major axis lines
    x_major_end = x0 + (major_axis / 2) * cos_theta
    y_major_end = y0 + (major_axis / 2) * sin_theta
    x_minor_end = x0 - (major_axis / 2) * cos_theta
    y_minor_end = y0 - (major_axis / 2) * sin_theta
    x_major_end2 = x02 + (major_axis2 / 2) * cos_theta2
    y_major_end2 = y02 + (major_axis2 / 2) * sin_theta2
    x_minor_end2 = x02 - (major_axis2 / 2) * cos_theta2
    y_minor_end2 = y02 - (major_axis2 / 2) * sin_theta2
    
    # minor axis lines
    x_minor_end_1 = x0 + (minor_axis / 2) * -sin_theta
    y_minor_end_1 = y0 + (minor_axis / 2) * cos_theta
    x_minor_end_2 = x0 - (minor_axis / 2) * -sin_theta
    y_minor_end_2 = y0 - (minor_axis / 2) * cos_theta
    x_minor_end_12 = x02 + (minor_axis2 / 2) * -sin_theta2
    y_minor_end_12 = y02 + (minor_axis2 / 2) * cos_theta2
    x_minor_end_22 = x02 - (minor_axis2 / 2) * -sin_theta2
    y_minor_end_22 = y02 - (minor_axis2 / 2) * cos_theta2
    
    # lines
    line_major = Line2D([x0, x_major_end], [y0, y_major_end], color='blue', linewidth=2)
    plt.gca().add_line(line_major)
    line_major = Line2D([x0, x_minor_end], [y0, y_minor_end], color='blue', linewidth=2)
    plt.gca().add_line(line_major)
    line_major2 = Line2D([x02, x_major_end2], [y02, y_major_end2], color='blue', linewidth=2)
    plt.gca().add_line(line_major2)
    line_major2 = Line2D([x02, x_minor_end2], [y02, y_minor_end2], color='blue', linewidth=2)
    plt.gca().add_line(line_major2)
    
    line_minor = Line2D([x0, x_minor_end_1], [y0, y_minor_end_1], color='green', linewidth=2)
    plt.gca().add_line(line_minor)
    line_minor = Line2D([x0, x_minor_end_2], [y0, y_minor_end_2], color='green', linewidth=2)
    plt.gca().add_line(line_minor)
    line_minor2 = Line2D([x02, x_minor_end_12], [y02, y_minor_end_12], color='green', linewidth=2)
    plt.gca().add_line(line_minor2)
    line_minor2 = Line2D([x02, x_minor_end_22], [y02, y_minor_end_22], color='green', linewidth=2)
    plt.gca().add_line(line_minor2)

    # annotate the sigmas
    plt.text(x_major_end, y_major_end, f'$\sigma_x={sigma_x:.2f}$', color='blue', fontsize=12, ha='center', va='center')
    plt.text(x_minor_end_1, y_minor_end_1, f'$\sigma_y={sigma_y:.2f}$', color='green', fontsize=12, ha='center', va='center')
    plt.text(x_major_end2, y_major_end2, f'$\sigma_x2={sigma_x2:.2f}$', color='blue', fontsize=12, ha='center', va='center')
    plt.text(x_minor_end_12, y_minor_end_12, f'$\sigma_y2={sigma_y2:.2f}$', color='green', fontsize=12, ha='center', va='center')
    
    plt.subplot(1, 2, 2)
    plt.imshow(fitted_gaussian, cmap='coolwarm', aspect='equal', vmin=0, vmax=1)
    plt.title('Fitted Gaussian Filtering Sensitivity')
    plt.colorbar(fraction=0.046, pad=0.04)

    plt.tight_layout()
    plt.show()

    # calculate circle radius of main RF
    if weighting_gaussian >= 0.5:
        major_axis = max(sigma_x, sigma_y) * 2.355
        minor_axis = min(sigma_x, sigma_y) * 2.355
        ellipse_surface = np.pi * (major_axis/2) * (minor_axis/2)
        circle_radius = np.sqrt(ellipse_surface/np.pi)
        print(f'circle radius of main RF: {circle_radius} degrees')
    else:
        major_axis = max(sigma_x2, sigma_y2) * 2.355
        minor_axis = min(sigma_x2, sigma_y2) * 2.355
        ellipse_surface = np.pi * (major_axis/2) * (minor_axis/2)
        circle_radius = np.sqrt(ellipse_surface/np.pi)
        print(f'circle radius of main RF: {circle_radius} degrees')
        
if len(centroids) == 3:
    initial_params = (centroids[0][1], centroids[0][0], 1, 1, 0, 0, centroids[1][1], centroids[1][0], 1, 1, 0, 0.5, centroids[2][1], centroids[2][0], 1, 1, 0, 0.5)
    result = minimize(negative_log_likelihood_triple, initial_params, args=(xy, gaussian_sensitivity, centroids), bounds = bounds_triple, method = 'L-BFGS-B') # , options={"disp": True})
    popt = result.x

    # define a higher resolution for the gaussian fit
    x = np.linspace(0, 20, 100)
    y = np.linspace(0, 20, 100)
    x, y = np.meshgrid(x, y)
    
    fitted_gaussian = gaussian_2d_triple((x, y), *popt).reshape(100,100)
    weighting_gaussian = popt[11]
    weighting_gaussian2 = popt[17]
    
    def gaussian_value_at_means(popt):
        xo, yo, sigma_x, sigma_y, theta, offset, xo2, yo2, sigma_x2, sigma_y2, theta2, weighting, xo3, yo3, sigma_x3, sigma_y3, theta3, weighting2 = popt
        amp = 1 - offset
        a = (np.cos(theta)**2)/(2*sigma_x**2) + (np.sin(theta)**2)/(2*sigma_y**2)
        b = -(np.sin(2*theta))/(4*sigma_x**2) + (np.sin(2*theta))/(4*sigma_y**2)
        c = (np.sin(theta)**2)/(2*sigma_x**2) + (np.cos(theta)**2)/(2*sigma_y**2)
        a2 = (np.cos(theta2)**2)/(2*sigma_x2**2) + (np.sin(theta2)**2)/(2*sigma_y2**2)
        b2 = -(np.sin(2*theta2))/(4*sigma_x2**2) + (np.sin(2*theta2))/(4*sigma_y2**2)
        c2 = (np.sin(theta2)**2)/(2*sigma_x2**2) + (np.cos(theta2)**2)/(2*sigma_y2**2)
        a3 = (np.cos(theta3)**2)/(2*sigma_x3**2) + (np.sin(theta3)**2)/(2*sigma_y3**2)
        b3 = -(np.sin(2*theta3))/(4*sigma_x3**2) + (np.sin(2*theta3))/(4*sigma_y3**2)
        c3 = (np.sin(theta3)**2)/(2*sigma_x3**2) + (np.cos(theta3)**2)/(2*sigma_y3**2)
        value_at_mean1 = offset + amp * (np.exp(0)*weighting + np.exp( - (a2*((xo-xo2)**2) + 2*b2*(xo-xo2)*(yo-yo2) + c2*((yo-yo2)**2)))*(1- weighting))
        value_at_mean2 = offset + amp * (np.exp( - (a*((xo2-xo)**2) + 2*b*(xo2-xo)*(yo2-yo) + c*((yo2-yo)**2)))*weighting + np.exp(0)*(1-weighting))
        value_at_mean3 = offset + amp * (np.exp( - (a*((xo2-xo)**2) + 2*b*(xo2-xo)*(yo2-yo) + c*((yo2-yo)**2)))*weighting + np.exp( - (a2*((xo-xo2)**2) + 2*b2*(xo-xo2)*(yo-yo2) + c2*((yo-yo2)**2)))*weighting2*(1- weighting) + np.exp(0)*(1-weighting2)*(1-weighting))
        return value_at_mean1, value_at_mean2, value_at_mean3  # exp(0) is 1, since we evaluate at the mean (xo, yo)

    # values for the fitted Gaussians at their means
    value_at_mean1_gaussian = gaussian_value_at_means(popt)[0]
    value_at_mean2_gaussian = gaussian_value_at_means(popt)[1]
    value_at_mean3_gaussian = gaussian_value_at_means(popt)[2]

    # normalize by the value at the 'primary' mean (the one cell we are recording from) and subtract the offset and then divide by the amplitude to get the actual gaussian only
    if weighting_gaussian > 1 - weighting_gaussian and weighting_gaussian > (1 - weighting_gaussian)*(1 - weighting_gaussian2):
        fitted_gaussian = ((fitted_gaussian / value_at_mean1_gaussian) - popt[5]) / (1 - popt[5])
    elif 1 - weighting_gaussian > weighting_gaussian and 1 - weighting_gaussian > (1 - weighting_gaussian)*(1 - weighting_gaussian2):
        fitted_gaussian = ((fitted_gaussian / value_at_mean2_gaussian) - popt[5]) / (1 - popt[5])
    elif (1 - weighting_gaussian)*(1 - weighting_gaussian2) > weighting_gaussian and (1 - weighting_gaussian)*(1 - weighting_gaussian2) > 1 - weighting_gaussian:
        fitted_gaussian = ((fitted_gaussian / value_at_mean3_gaussian) - popt[5]) / (1 - popt[5])

    # theta represents the angle of the major axis and the x-axis for the three gaussians
    def adjust_theta(popt):
        sigma_x, sigma_y, sigma_x2, sigma_y2, sigma_x3, sigma_y3 = popt[2], popt[3], popt[8], popt[9], popt[14], popt[15]
        if sigma_x < sigma_y:
            popt[2], popt[3] = sigma_y, sigma_x  # swap sigma_x and sigma_y
            popt[4] += np.pi / 2  # adjust theta by 90 degrees
        popt[4] = np.mod(popt[4], np.pi)  # constrain theta to [0, π)
        if sigma_x2 < sigma_y2:
            popt[8], popt[9] = sigma_y2, sigma_x2  # swap sigma_x2 and sigma_y2
            popt[10] += np.pi / 2  # adjust theta2 by 90 degrees
        popt[10] = np.mod(popt[10], np.pi)  # constrain theta2 to [0, π)
        if sigma_x3 < sigma_y3:
            popt[14], popt[15] = sigma_y3, sigma_x3  # swap sigma_x3 and sigma_y3
            popt[16] += np.pi / 2  # adjust theta3 by 90 degrees
        popt[16] = np.mod(popt[16], np.pi)  # constrain theta3 to [0, π)
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
    print(f"x02: {popt[6]}")
    print(f"y02: {popt[7]}")
    print(f"sigma_x2: {popt[8]}")
    print(f"sigma_y2: {popt[9]}")
    print(f"theta2: {popt[10]}")
    print(f"weighting: {popt[11]}")
    print(f"x03: {popt[12]}")
    print(f"y03: {popt[13]}")
    print(f"sigma_x3: {popt[14]}")
    print(f"sigma_y3: {popt[15]}")
    print(f"theta3: {popt[16]}")
    print(f"weighting2: {popt[17]}")

    plt.figure(figsize=(15, 10))

    plt.subplot(1, 2, 1)
    plt.imshow(gaussian_sensitivity, cmap='coolwarm', aspect='equal', vmin=0, vmax=1)
    plt.title('Gaussian Filtering Sensitivity')
    plt.colorbar(fraction=0.046, pad=0.04)

    # parameters for the ellipses
    x0, y0, x02, y02, x03, y03 = popt[0], popt[1], popt[6], popt[7], popt[12], popt[13]
    sigma_x, sigma_y, sigma_x2, sigma_y2, sigma_x3, sigma_y3 = popt[2], popt[3], popt[8], popt[9], popt[14], popt[15]
    theta, theta2, theta3 = popt[4], popt[10], popt[16]
    major_axis = max(sigma_x, sigma_y) * 2.355
    minor_axis = min(sigma_x, sigma_y) * 2.355
    major_axis2 = max(sigma_x2, sigma_y2) * 2.355
    minor_axis2 = min(sigma_x2, sigma_y2) * 2.355
    major_axis3 = max(sigma_x3, sigma_y3) * 2.355
    minor_axis3 = min(sigma_x3, sigma_y3) * 2.355
    
    # ellipses
    ellipse = Ellipse((x0, y0), width=major_axis, height=minor_axis, angle=-np.degrees(theta), edgecolor='r', fc='None', lw=2)
    ellipse2 = Ellipse((x02, y02), width=major_axis2, height=minor_axis2, angle=-np.degrees(theta2), edgecolor='r', fc='None', lw=2)
    ellipse3 = Ellipse((x03, y03), width=major_axis3, height=minor_axis3, angle=-np.degrees(theta3), edgecolor='r', fc='None', lw=2)
    plt.gca().add_patch(ellipse)
    plt.gca().add_patch(ellipse2)
    plt.gca().add_patch(ellipse3)
    
    # endpoints of the lines representing sigma_x and sigma_y
    cos_theta = np.cos(-theta)
    sin_theta = np.sin(-theta)
    cos_theta2 = np.cos(-theta2)
    sin_theta2 = np.sin(-theta2)
    cos_theta3 = np.cos(-theta3)
    sin_theta3 = np.sin(-theta3)
    
    # major axis lines
    x_major_end = x0 + (major_axis / 2) * cos_theta
    y_major_end = y0 + (major_axis / 2) * sin_theta
    x_minor_end = x0 - (major_axis / 2) * cos_theta
    y_minor_end = y0 - (major_axis / 2) * sin_theta
    x_major_end2 = x02 + (major_axis2 / 2) * cos_theta2
    y_major_end2 = y02 + (major_axis2 / 2) * sin_theta2
    x_minor_end2 = x02 - (major_axis2 / 2) * cos_theta2
    y_minor_end2 = y02 - (major_axis2 / 2) * sin_theta2
    x_major_end3 = x03 + (major_axis3 / 2) * cos_theta3
    y_major_end3 = y03 + (major_axis3 / 2) * sin_theta3
    x_minor_end3 = x03 - (major_axis3 / 2) * cos_theta3
    y_minor_end3 = y03 - (major_axis3 / 2) * sin_theta3
    
    # minor axis lines
    x_minor_end_1 = x0 + (minor_axis / 2) * -sin_theta
    y_minor_end_1 = y0 + (minor_axis / 2) * cos_theta
    x_minor_end_2 = x0 - (minor_axis / 2) * -sin_theta
    y_minor_end_2 = y0 - (minor_axis / 2) * cos_theta
    x_minor_end_12 = x02 + (minor_axis2 / 2) * -sin_theta2
    y_minor_end_12 = y02 + (minor_axis2 / 2) * cos_theta2
    x_minor_end_22 = x02 - (minor_axis2 / 2) * -sin_theta2
    y_minor_end_22 = y02 - (minor_axis2 / 2) * cos_theta2
    x_minor_end_13 = x03 + (minor_axis3 / 2) * -sin_theta3
    y_minor_end_13 = y03 + (minor_axis3 / 2) * cos_theta3
    x_minor_end_23 = x03 - (minor_axis3 / 2) * -sin_theta3
    y_minor_end_23 = y03 - (minor_axis3 / 2) * cos_theta3
    
    # lines
    line_major = Line2D([x0, x_major_end], [y0, y_major_end], color='blue', linewidth=2)
    plt.gca().add_line(line_major)
    line_major = Line2D([x0, x_minor_end], [y0, y_minor_end], color='blue', linewidth=2)
    plt.gca().add_line(line_major)
    line_major2 = Line2D([x02, x_major_end2], [y02, y_major_end2], color='blue', linewidth=2)
    plt.gca().add_line(line_major2)
    line_major2 = Line2D([x02, x_minor_end2], [y02, y_minor_end2], color='blue', linewidth=2)
    plt.gca().add_line(line_major2)
    line_major3 = Line2D([x03, x_major_end3], [y03, y_major_end3], color='blue', linewidth=2)
    plt.gca().add_line(line_major3)
    line_major3 = Line2D([x03, x_minor_end3], [y03, y_minor_end3], color='blue', linewidth=2)
    plt.gca().add_line(line_major3)
    
    line_minor = Line2D([x0, x_minor_end_1], [y0, y_minor_end_1], color='green', linewidth=2)
    plt.gca().add_line(line_minor)
    line_minor = Line2D([x0, x_minor_end_2], [y0, y_minor_end_2], color='green', linewidth=2)
    plt.gca().add_line(line_minor)
    line_minor2 = Line2D([x02, x_minor_end_12], [y02, y_minor_end_12], color='green', linewidth=2)
    plt.gca().add_line(line_minor2)
    line_minor2 = Line2D([x02, x_minor_end_22], [y02, y_minor_end_22], color='green', linewidth=2)
    plt.gca().add_line(line_minor2)
    line_minor3 = Line2D([x03, x_minor_end_13], [y03, y_minor_end_13], color='green', linewidth=2)
    plt.gca().add_line(line_minor3)
    line_minor3 = Line2D([x03, x_minor_end_23], [y03, y_minor_end_23], color='green', linewidth=2)
    plt.gca().add_line(line_minor3)

    # annotate the sigmas
    plt.text(x_major_end, y_major_end, f'$\sigma_x={sigma_x:.2f}$', color='blue', fontsize=12, ha='center', va='center')
    plt.text(x_minor_end_1, y_minor_end_1, f'$\sigma_y={sigma_y:.2f}$', color='green', fontsize=12, ha='center', va='center')
    plt.text(x_major_end2, y_major_end2, f'$\sigma_x2={sigma_x2:.2f}$', color='blue', fontsize=12, ha='center', va='center')
    plt.text(x_minor_end_12, y_minor_end_12, f'$\sigma_y2={sigma_y2:.2f}$', color='green', fontsize=12, ha='center', va='center')
    plt.text(x_major_end3, y_major_end3, f'$\sigma_x3={sigma_x3:.2f}$', color='blue', fontsize=12, ha='center', va='center')
    plt.text(x_minor_end_13, y_minor_end_13, f'$\sigma_y3={sigma_y3:.2f}$', color='green', fontsize=12, ha='center', va='center')
    
    plt.subplot(1, 2, 2)
    plt.imshow(fitted_gaussian, cmap='coolwarm', aspect='equal', vmin=0, vmax=1)
    plt.title('Fitted Gaussian Filtering Sensitivity')
    plt.colorbar(fraction=0.046, pad=0.04)

    plt.tight_layout()
    plt.show()

    # calculate circle radius of RF
    if weighting_gaussian > 1 - weighting_gaussian and weighting_gaussian > (1 - weighting_gaussian)*(1 - weighting_gaussian2):
        major_axis = max(sigma_x, sigma_y) * 2.355
        minor_axis = min(sigma_x, sigma_y) * 2.355
        ellipse_surface = np.pi * (major_axis/2) * (minor_axis/2)
        circle_radius = np.sqrt(ellipse_surface/np.pi)
        print(f'circle radius of main RF: {circle_radius} degrees')
    elif 1 - weighting_gaussian > weighting_gaussian and 1 - weighting_gaussian > (1 - weighting_gaussian)*(1 - weighting_gaussian2):
        major_axis = max(sigma_x2, sigma_y2) * 2.355
        minor_axis = min(sigma_x2, sigma_y2) * 2.355
        ellipse_surface = np.pi * (major_axis/2) * (minor_axis/2)
        circle_radius = np.sqrt(ellipse_surface/np.pi)
        print(f'circle radius of main RF: {circle_radius} degrees')
    elif (1 - weighting_gaussian)*(1 - weighting_gaussian2) > weighting_gaussian and (1 - weighting_gaussian)*(1 - weighting_gaussian2) > 1 - weighting_gaussian:
        major_axis = max(sigma_x3, sigma_y3) * 2.355
        minor_axis = min(sigma_x3, sigma_y3) * 2.355
        ellipse_surface = np.pi * (major_axis/2) * (minor_axis/2)
        circle_radius = np.sqrt(ellipse_surface/np.pi)
        print(f'circle radius of main RF: {circle_radius} degrees')

    
