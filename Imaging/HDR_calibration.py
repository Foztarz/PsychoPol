import sys
import os
import re
import numpy as np
import matplotlib.pyplot as plt
from skimage import io
from skimage.draw import disk
from sklearn.linear_model import LinearRegression
from scipy.optimize import curve_fit

def extract_number_from_filename(filename):
    """
    Extract the number between '--' and 'us' in the filename.
    """
    match = re.search(r'--([0-9.]+)us', filename)
    if match:
        return float(match.group(1))
    return None

def calculate_average_intensity(image, center, radius):
    """
    Calculate the average pixel intensity within a circular region.
    """
    rr, cc = disk(center, radius, shape=image.shape)
    return np.mean(image[rr, cc])

def draw_circles_on_image(image, circles, radius):
    """
    Draw circles on the image to visualize the regions where the intensity is calculated.
    """
    fig, ax = plt.subplots(figsize=(10, 10))
    ax.imshow(image, cmap='gray')
    
    for circle in circles:
        center = circle["center"]
        label = circle["label"]
        # draw a circle
        circle_artist = plt.Circle(center[::-1], radius, color='red', fill=False, linewidth=2)
        ax.add_artist(circle_artist)
        # label near the circle
        ax.text(center[1] + 10, center[0] - 10, label, color='red', fontsize=12, weight='bold')
    
    plt.title("Image with Regions of Interest (ROI) Marked")
    plt.axis('off')
    plt.show()

def main(folder_path):
    if not os.path.exists(folder_path):
        print("The provided folder path does not exist.")
        return

    # the circle parameters and labels

    circles = [
        {"center": (942, 865), "label": "99% Reflectance", "reflectance": 0.99},
        {"center": (910, 1092), "label": "75% Reflectance", "reflectance": 0.75},
        {"center": (929, 1323), "label": "50% Reflectance", "reflectance": 0.50},
        {"center": (989, 1522), "label": "0.2% Reflectance", "reflectance": 0.002},
        {"center": (100, 100), "label": "control", "reflectance": 0.002}
    ]
    circles_sample = [
        {"center": (942, 865), "label": "99% Reflectance", "reflectance": 0.99},
        {"center": (910, 1092), "label": "75% Reflectance", "reflectance": 0.75},
        {"center": (929, 1323), "label": "50% Reflectance", "reflectance": 0.50},
    ]

    radius = 20

    # data storage for plotting
    extracted_values = []
    intensities = {circle["label"]: [] for circle in circles}

    # process each file in the folder
    for i, file in enumerate(sorted(os.listdir(folder_path))):
        if file.endswith(".tiff"):
            filepath = os.path.join(folder_path, file)
            exposure_time = extract_number_from_filename(file)
            if exposure_time is None:
                continue

            image = io.imread(filepath)
            
            # draw circles on the first image only to avoid repeated display
            if i == 0:
                draw_circles_on_image(image, circles, radius)

            # calculate average intensities for the defined circles
            for circle in circles:
                avg_intensity = calculate_average_intensity(image, circle["center"], radius)
                intensities[circle["label"]].append(avg_intensity)

            # exposure time
            extracted_values.append(exposure_time)

    # scatter plot all data on the same plot
    for label, values in intensities.items():
        plt.scatter(
            extracted_values, 
            values, 
            label=label,
            alpha=0.7
        )
        
    plt.xlabel("Exposure Time in us (at 2Hz frame rate)")
    plt.ylabel("Average px Intensity")
    plt.title("Average px Intensity Across Standards at Various Exposure Times")
    plt.legend()
    plt.grid(True)
    plt.show()

    ratios = {label: [] for label in intensities if label != "0.2% Reflectance"}  # No ratio for the 0.2% itself
    intensities_0_2 = intensities["0.2% Reflectance"]

    for label in ratios.keys():
        # ratio of each intensity to the 0.2% reflectance intensity
        for i in range(len(intensities_0_2)):
            if intensities_0_2[i] != 0:  # Avoid division by zero
                ratio = intensities[label][i] / intensities_0_2[i]
            else:
                ratio = np.nan  # ff the 0.2% reflectance intensity is zero, store NaN
            ratios[label].append(ratio)
    
    for label, values in ratios.items():
        plt.scatter(
            extracted_values, 
            values, 
            label=f'{label} / 0.2% Reflectance',
            alpha=0.7
        )

    plt.xlabel("Exposure Time in us (at 2Hz frame rate)")
    plt.ylabel("Ratio of Intensity to 0.2% Reflectance")
    plt.legend()
    plt.grid(True)
    plt.show()

    for circle in circles:
        label = circle["label"]
        reflectance = circle["reflectance"]

        reflectance_times_exposure = [reflectance * et for et in extracted_values]
        gray_values = intensities[label]

        plt.scatter(
            reflectance_times_exposure, 
            gray_values, 
            label=label,
            alpha=0.7
        )

    plt.xlabel("Reflectance × Exposure Time")
    plt.ylabel("Gray Value")
    plt.title("Gray Value vs Reflectance × Exposure Time")
    plt.legend()
    plt.grid(True)
    plt.show()

    # second plot: filtered data and regression line
    filtered_x = []
    filtered_y = []

    for circle in circles_sample:
        label = circle["label"]
        reflectance = circle["reflectance"]

        reflectance_times_exposure = np.array([reflectance * et for et in extracted_values])
        gray_values = np.array(intensities[label])

        # filter data points based on y-value range CHANGE IF NEEDED
        mask = (gray_values >= 0) & (gray_values <= 255)
        filtered_x.extend(reflectance_times_exposure[mask])
        filtered_y.extend(gray_values[mask])

    filtered_x = np.array(filtered_x).reshape(-1, 1)  # reshape for sklearn
    filtered_y = np.array(filtered_y)

    # regression model
    reg = LinearRegression()
    reg.fit(filtered_x, filtered_y)
    y_pred = reg.predict(filtered_x)

    # R^2
    r_squared = reg.score(filtered_x, filtered_y)

    plt.scatter(filtered_x, filtered_y, color="blue", alpha=0.7, label="Filtered Data")
    plt.plot(filtered_x, y_pred, color="red", label=f"Regression Line (R²={r_squared:.4f})")

    plt.xlabel("Reflectance × Exposure Time")
    plt.ylabel("Gray Value")
    plt.title("Filtered Data with Regression Line")
    plt.legend()
    plt.grid(True)
    plt.show()

    print(f"R^2: {r_squared:.4f}")

    def sigmoid(x, a, b, c, d):
        return a / (1 + np.exp(-b * (x - c))) + d

    # normalize x values to improve fitting
    x_min, x_max = np.min(filtered_x), np.max(filtered_x)
    filtered_x_norm = (filtered_x - x_min) / (x_max - x_min)  # normalize to [0, 1]

    # initial parameter estimates: 
    # a: max value of y, b: steepness, c: midpoint, d: min value of y
    initial_params = [max(filtered_y), 1, 0.5, min(filtered_y)]

    # sigmoid function
    popt, _ = curve_fit(sigmoid, filtered_x_norm.ravel(), filtered_y, p0=initial_params, maxfev=10000)

    # fitted y-values
    x_range = np.linspace(0, 1, 500)  # Use normalized range for fitting
    y_fit = sigmoid(x_range, *popt)

    # convert x_range back to original scale for plotting
    x_range_original = x_range * (x_max - x_min) + x_min

    plt.scatter(filtered_x, filtered_y, color="blue", alpha=0.7, label="Filtered Data")
    plt.plot(x_range_original, y_fit, color="green", linewidth=2, label="Sigmoid Fit")

    plt.xlabel("Reflectance × Exposure Time")
    plt.ylabel("Gray Value")
    plt.title("Full Data with Sigmoid Fit")
    plt.legend()
    plt.grid(True)
    plt.show()

    print(f"Optimized parameters: {popt}")



if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python script.py <folder_path>")
    else:
        main(sys.argv[1])
