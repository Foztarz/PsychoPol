import sys
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt


## IMPORTANT: EYE ANGLES INCREASE CLOCKWISE FROM UP, SUN AZIMUTH INCREASES COUNTERCLOCKWISE FROM UP


if len(sys.argv) != 3:
    print("Usage: python sun_vector_histogram.py <data_file.tsv> <starting_sun_angle>")
    sys.exit(1)

file_path = sys.argv[1]
start_sun_angle = float(sys.argv[2])  

data = pd.read_csv(file_path, sep='\t')

def polar_to_cartesian(angle_deg, length):
    angle_rad = np.radians(angle_deg)
    x = length * np.cos(angle_rad)
    y = length * np.sin(angle_rad)
    return x, y

# vector components
fx, fy = polar_to_cartesian(data['First Eye Angle'], data['First Eye Vector Length'])
sx, sy = polar_to_cartesian(data['Second Eye Angle'], data['Second Eye Vector Length'])

# Combine vectors
total_x = fx + sx
total_y = fy + sy
total_magnitude = np.sqrt(total_x**2 + total_y**2)
total_angle_rad = np.arctan2(total_y, total_x)

# convert to polar plot frame (0 = front = down = south)
polar_angles = np.pi - total_angle_rad  # flip to make 0 rad = 180° (front)

# first plot: polar histogram of combined vectors
plt.figure(figsize=(8, 8))
ax1 = plt.subplot(111, polar=True)

num_bins = 72
ax1.hist(polar_angles, bins=num_bins, weights=total_magnitude,
         alpha=0.75, color='royalblue', edgecolor='k')

ax1.set_theta_zero_location("S")  # Front is down
ax1.set_theta_direction(1)        # Counterclockwise
ax1.set_title("Polar Histogram of Combined Eye Vectors", va='top')

plt.tight_layout()
plt.show()

# second plot: polar histogram of angular error per rotation

num_trials = len(data)

# compute true sun angle for each rotation
sun_angles_deg = np.array([start_sun_angle + i * -5 for i in range(num_trials)])
sun_angles_rad = np.radians(sun_angles_deg)

# angular error (wrapped to [-π, π]) and convert to degrees
angle_diff = total_angle_rad - sun_angles_rad
angle_diff = (angle_diff + np.pi) % (2 * np.pi) - np.pi
abs_error_deg = np.abs(np.degrees(angle_diff))

# convert sun angles to polar plot frame (front = 0 deg)
plot_angles = np.pi - sun_angles_rad

plt.figure(figsize=(8, 8))
ax2 = plt.subplot(111, polar=True)

bar_width = (2 * np.pi) / 72  # match visual size
ax2.bar(plot_angles, abs_error_deg, width=bar_width,
        alpha=0.8, color='crimson', edgecolor='k')

ax2.set_theta_zero_location("S")
ax2.set_theta_direction(1)
ax2.set_title("Angular Error (Degrees) per Trial", va='top')

plt.tight_layout()
plt.show()



