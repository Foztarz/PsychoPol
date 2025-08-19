import numpy as np
import matplotlib.pyplot as plt
from scipy.optimize import curve_fit
import sys
from itertools import cycle

# usage: python cos2_pol_sensitivities_multiple.py <response_data>

# cosine squared function
def cosine_squared(x, A, B, C, D):
    x = np.deg2rad(x)
    return A * np.cos(B * x + C)**2 + D

# data
data = np.loadtxt(sys.argv[1])  # expecting: degrees, response1, response2, ...
x_data = data[:, 0]
y_columns = data[:, 1:]  # all response columns

# colors for plotting
color_cycle = cycle(plt.cm.tab10(np.linspace(0, 1, y_columns.shape[1])))

#  smooth curve
x_fine = np.linspace(min(x_data), max(x_data), 1000)

plt.figure()


font_size = 18
tick_label_size = 16
y_tick_size = 18



# fit and plot each response column
for idx in range(y_columns.shape[1]):
    y_data = y_columns[:, idx]

    # initial guess for fitting
    initial_guess = [16, 1, 0, 0]

    try:
        color = next(color_cycle)

        params, _ = curve_fit(cosine_squared, x_data, y_data, p0=initial_guess, maxfev=10000) # fit
        fitted_y_fine = cosine_squared(x_fine, *params)

        max_y = np.max(fitted_y_fine)
        min_y = np.min(fitted_y_fine)
        x_at_max = x_fine[np.argmax(fitted_y_fine)]
        x_at_min = x_fine[np.argmin(fitted_y_fine)]

        # calculate the period in degrees
        period = 180 / params[1]

        # wrap max x position to [0, 180] range
        adjusted_x_at_max = x_at_max
        while adjusted_x_at_max > 180:
            adjusted_x_at_max -= period


        print(f"Curve {idx+1} - Fitted parameters:", params)
        print(f"Curve {idx+1} - Max: {max_y:.2f} at {x_at_max:.2f}, Min: {min_y:.2f} at {x_at_min:.2f}")

        # plot
        plt.axvline(x=adjusted_x_at_max, color=color, linestyle='--', linewidth=1)
        plt.scatter([adjusted_x_at_max], [max_y], color=color, edgecolor='black', marker='^', zorder=5)
        plt.annotate(
            f'{adjusted_x_at_max:.1f}°',
            xy=(adjusted_x_at_max, max_y),
            xytext=(adjusted_x_at_max + 3, max_y),
            fontsize=12,
            color=color
        )
        
        plt.plot(x_fine, fitted_y_fine, color=color, label=f'Fit {idx+1}')
        plt.scatter(x_data, y_data, color=color, s=10, alpha=0.5)
        plt.scatter([x_at_max], [max_y], color=color, edgecolor='black', marker='^', zorder=5)

    except RuntimeError:
        print(f"Curve {idx+1} - Fit did not converge")

plt.xlabel('Degrees', fontsize=font_size)
plt.ylabel('Polarization Sensitivity', fontsize=font_size)
plt.xticks(fontsize=tick_label_size)
plt.yticks(fontsize=y_tick_size)
plt.legend(fontsize=font_size)
plt.tight_layout()
plt.show()
