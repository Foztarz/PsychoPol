import numpy as np
import sys
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation
from scipy.optimize import curve_fit

# === Load and parse calibration data ===
calibration_data = np.loadtxt(sys.argv[1], delimiter='\t')
mm_distances = calibration_data[:, 0]
px_distances = calibration_data[:, 1]

# Sort and deduplicate if needed
unique_indices = np.unique(mm_distances, return_index=True)[1]
mm_distances = mm_distances[unique_indices]
px_distances = px_distances[unique_indices]
sort_indices = np.argsort(mm_distances)
mm_distances = mm_distances[sort_indices]
px_distances = px_distances[sort_indices]

# === Fit model: px = a * (1 - exp(-b * mm)) ===
def forward_model(mm, a, b):
    return a * (1 - np.exp(-b * mm))

initial_guess = [np.max(px_distances), 0.01]
params, _ = curve_fit(forward_model, mm_distances, px_distances, p0=initial_guess)
a, b = params

# Inverse model: mm = -ln(1 - px / a) / b
def inverse_model(px):
    with np.errstate(divide='ignore', invalid='ignore'):
        return -np.log(1 - px / a) / b

# === Load tracking coordinates ===
coords = np.loadtxt(sys.argv[2], delimiter=',')  # x, y per line
x_coords = coords[:, 0]
y_coords = coords[:, 1]

# === Undistort coordinates ===
# Use image parameters that match the image used in undistortion
IMAGE_WIDTH = 1280
IMAGE_HEIGHT = 720
center_x = IMAGE_WIDTH / 2
center_y = IMAGE_HEIGHT / 2

# Compute distances from image center
dx = x_coords - center_x
dy = y_coords - center_y
distorted_radii = np.sqrt(dx**2 + dy**2)

# Undistort: px (distorted radius) → mm (undistorted radius)
undistorted_radii = inverse_model(distorted_radii)

# Apply radial correction
scaling = np.ones_like(distorted_radii)
nonzero = distorted_radii > 0
scaling[nonzero] = undistorted_radii[nonzero] / distorted_radii[nonzero]

undistorted_x = center_x + dx * scaling
undistorted_y = center_y - dy * scaling

# Flip Y to match image coordinates
distorted_y_flipped = y_coords
undistorted_y_flipped = IMAGE_HEIGHT - undistorted_y

# === Plot exponential model fit ===
fit_mm = np.linspace(np.min(mm_distances), np.max(mm_distances), 300)
fit_px = forward_model(fit_mm, a, b)

plt.figure()
plt.scatter(mm_distances, px_distances, label="Calibration Data")
plt.plot(fit_mm, fit_px, 'r-', label=f"Fit: px = {a:.2f}·(1 - exp(-{b:.4f}·mm))")
plt.xlabel("Real-World Distance (mm)")
plt.ylabel("Pixel Distance (distorted)")
plt.title("Distortion Model Fit")
plt.legend()
plt.grid(True)
plt.tight_layout()
plt.show()

# === Animation of distorted vs. undistorted paths ===
fig, ax = plt.subplots()
ax.set_xlim(0, IMAGE_WIDTH)
ax.set_ylim(0, IMAGE_HEIGHT)
ax.invert_yaxis()  # Origin at top-left to match image

# Plot static distorted path
ax.plot(x_coords, distorted_y_flipped, color='gray', alpha=0.4, label="Distorted Path")

# Animated undistorted path
trail = ax.scatter([], [], s=20, c=[], cmap='viridis', vmin=0, vmax=len(undistorted_x)-1, label="Undistorted Trail")

ax.set_title("Distorted vs Undistorted Trajectory")
ax.set_xlabel("X (pixels)")
ax.set_ylabel("Y (pixels)")
ax.legend()

def update(frame):
    trail_x = undistorted_x[:frame+1]
    trail_y = undistorted_y_flipped[:frame+1]
    trail_colors = np.arange(frame+1)
    trail.set_offsets(np.column_stack([trail_x, trail_y]))
    trail.set_array(trail_colors)
    return trail,

ani = FuncAnimation(fig, update, frames=len(undistorted_x), interval=1, blit=True, repeat=False)
plt.tight_layout()
plt.show()
