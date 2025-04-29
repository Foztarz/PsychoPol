import sys
import numpy as np
import matplotlib.pyplot as plt

# correct usage
if len(sys.argv) != 2:
    print("Usage: python script.py <FWHM_pol_sens_data>")
    sys.exit(1)

# file path from the command-line argument
file_path = sys.argv[1]

# lists to hold data, 2 columns (FWHM, pol_sens)
x_values = []
y_values = []
categories = []

# read the file and populate the data lists
with open(file_path, 'r') as file:
    for line in file:
        # skip empty lines
        if not line.strip():
            continue
        try:
            y, x, category = line.strip().split()
            x_values.append(float(x))
            y_values.append(float(y))
            categories.append(category)
        except ValueError:
            print(f"Skipping malformed line: {line.strip()}")
            continue

# convert lists to numpy arrays
x_values = np.array(x_values)
y_values = np.array(y_values)
categories = np.array(categories)

# define distinct colors for each category
category_colors = {
    'main_retina': '#FFD700',
    'marginal': '#FF8C00',
    'DRA': '#2E8B57',
}

# scatter plot
plt.figure(figsize=(10, 10))

# loop through each category and plot
for category in np.unique(categories):
    indices = np.where(categories == category)
    plt.scatter(x_values[indices], y_values[indices],
                label=category,
                alpha=0.8,
                edgecolor='black',
                s=100,
                color=category_colors.get(category, '#000000'))

plt.tick_params(axis='both', labelsize=16)

# grid
plt.grid(True)

plt.legend(fontsize=14)

plt.savefig('scatter_plot.png')

plt.show()
