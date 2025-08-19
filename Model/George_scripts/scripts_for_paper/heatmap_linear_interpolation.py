import sys
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from scipy.interpolate import griddata

def main():
    if len(sys.argv) != 2:
        print("Usage: python heatmap.py <data.tsv>")
        sys.exit(1)

    filepath = sys.argv[1]

    # Load TSV file
    df = pd.read_csv(filepath, sep="\t", header=0)

    # Group duplicates and average errors
    grouped = df.groupby(['sun_elevation', 'sun_azimuth'], as_index=False).mean()

    # Convert to polar
    r = grouped['sun_elevation'].values
    theta_deg = grouped['sun_azimuth'].values
    theta = np.radians(theta_deg)
    errors = grouped['error'].values

    # Convert to Cartesian
    x = r * np.cos(theta)
    y = r * np.sin(theta)

    # Create a Cartesian grid
    grid_x, grid_y = np.meshgrid(
        np.linspace(x.min(), x.max(), 300),
        np.linspace(y.min(), y.max(), 300)
    )

    # Interpolate error values
    error_grid = griddata(
        points=(x, y),
        values=errors,
        xi=(grid_x, grid_y),
        method='linear',
        fill_value=np.nan
    )

    # Remove negative/interpolation artifacts
    error_grid = np.clip(error_grid, 0, np.nanmax(errors))

    # Convert grid to polar for plotting
    r_grid = np.sqrt(grid_x**2 + grid_y**2)
    theta_grid = np.arctan2(grid_y, grid_x)

    # Plot
    fig, ax = plt.subplots(subplot_kw=dict(projection='polar'), figsize=(8, 8))

    cmap_plot = ax.pcolormesh(theta_grid, r_grid, error_grid, cmap='RdYlBu_r', shading='nearest')
    ax.scatter(theta, r, facecolors='none', edgecolors='k', s=10, alpha=0.5, label='Data Points')

    ax.set_theta_zero_location('N')
    ax.set_theta_direction(-1)

    cbar = plt.colorbar(cmap_plot, ax=ax, label=' Sun azimuth estimate - Error (deg)')
    ax.legend(loc='lower right')
    
    plt.show()

if __name__ == "__main__":
    main()
