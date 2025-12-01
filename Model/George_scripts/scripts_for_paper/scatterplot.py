import sys
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

def assign_error_colors(errors, cmap_name='viridis', vmin=0, vmax=180):
    """Map error values to colors using a fixed 0–180° range colormap."""
    cmap = plt.get_cmap(cmap_name)
    norm = plt.Normalize(vmin=vmin, vmax=vmax)
    return cmap(norm(errors)), cmap, norm

def assign_quadrant_colors(angles):
    """
    Assign a color and label for each quadrant:
    Quadrant I   (0-90)   : green, left-posterior
    Quadrant II  (90-180) : red, left-anterior
    Quadrant III (180-270): blue, right-anterior
    Quadrant IV  (270-360): orange, right-posterior
    """
    angles = angles % 360  # normalize to 0-360
    conditions = [
        (angles >= 0) & (angles < 90),
        (angles >= 90) & (angles < 180),
        (angles >= 180) & (angles < 270),
        (angles >= 270) & (angles < 360)
    ]
    colors = ['green', 'red', 'blue', 'orange']
    labels = ['left-posterior', 'left-anterior', 'right-anterior', 'right-posterior']
    return np.select(conditions, colors), np.select(conditions, labels)

def main():
    if len(sys.argv) < 2:
        print("Usage: python scatterplot.py <file.tsv>")
        sys.exit(1)

    file_path = sys.argv[1]

    # Load TSV into DataFrame
    df = pd.read_csv(file_path, sep="\t", header=None)

    if df.shape[1] < 3:
        print("Error: The TSV file must contain at least three columns.")
        sys.exit(1)

    x_col, y_col, angle_col = df.columns[:3]

    # Quadrant colors for Cartesian plot
    quadrant_colors, quadrant_labels = assign_quadrant_colors(df[angle_col])

    # Error colors for polar plot (standardized 0–180)
    error_colors, cmap, norm = assign_error_colors(df[y_col])

    # ======================
    # FIGURE 1: CARTESIAN
    # ======================
    fig1, ax1 = plt.subplots(figsize=(7, 6))
    x_vals = np.abs(np.degrees(df[x_col]))
    y_vals = df[y_col]

    ax1.scatter(x_vals, y_vals, c=quadrant_colors, alpha=0.7)
    ax1.set_xlabel("fanshape_mismatch (degrees)")
    ax1.set_ylabel("error (degrees)")
    ax1.set_title("Cartesian Scatter Plot by Quadrant")
    ax1.axhline(0, color='gray', linestyle='--', linewidth=1)
    ax1.axvline(0, color='gray', linestyle='--', linewidth=1)
    ax1.grid(True, linestyle=':')

    # Standardized axis limits
    #ax1.set_xlim(0, 45)
    #ax1.set_ylim(0, 180)

    # Custom legend
    unique_labels = ['left-posterior', 'left-anterior', 'right-anterior', 'right-posterior']
    unique_colors = ['green', 'red', 'blue', 'orange']
    handles = [plt.Line2D([0], [0], marker='o', color='w', label=lbl,
                          markerfacecolor=col, markersize=10)
               for lbl, col in zip(unique_labels, unique_colors)]
    ax1.legend(handles=handles, title="Quadrants")

    plt.tight_layout()
    plt.show()

    # ======================
    # FIGURE 2: POLAR
    # ======================
    fig2, ax2 = plt.subplots(figsize=(7, 6), subplot_kw={'polar': True})
    theta = np.radians(df[angle_col] % 360)
    r = np.abs(np.degrees(df[x_col]))
    errors = df[y_col]

    sc = ax2.scatter(theta, r, c=errors, cmap=cmap, norm=norm, alpha=0.8)
    cbar = plt.colorbar(sc, ax=ax2, pad=0.1)
    cbar.set_label("Error (degrees)")
    cbar.set_ticks(np.linspace(0, 180, 7))  # 0, 30, 60, 90, 120, 150, 180

    ax2.set_title("Polar Plot (r=fanshape_mismatch (degrees), θ=sun azimuth)")
    ax2.set_theta_zero_location("N")  # 0° up (north)
    ax2.set_theta_direction(-1)
    ax2.set_rlabel_position(135)

    # Standardize radial axis to 0–45
    ax2.set_rlim(0, 45)

    # Optional: quadrant lines
    for deg in [0, 90, 180, 270]:
        ax2.plot([np.radians(deg), np.radians(deg)], [0, 45],
                 color='gray', linestyle='--', linewidth=0.8)

    plt.tight_layout()
    plt.show()

if __name__ == "__main__":
    main()
