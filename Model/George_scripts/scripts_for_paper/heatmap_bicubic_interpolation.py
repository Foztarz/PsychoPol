import sys
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from scipy.interpolate import griddata, RectBivariateSpline

def inverse_softplus(y):
    return np.log(np.exp(y) - 1)

def softplus(x):
    return np.log1p(np.exp(x))

def main():
    if len(sys.argv) != 2:
        print("Usage: python heatmap.py <data.tsv>")
        sys.exit(1)

    filepath = sys.argv[1]

    # Load TSV file
    df = pd.read_csv(filepath, sep="\t", header=0)

    # Group duplicates and average errors if same sun azimuth/elevation
    grouped = df.groupby(['sun_elevation', 'sun_azimuth'], as_index=False).mean()

    # Convert to polar
    r = grouped['sun_elevation'].values
    r_max = 90  # typically 90 for zenith
    r_plot = r_max - r   # 0° elev → outer edge, 90° elev → center

    theta_deg = grouped['sun_azimuth'].values
    theta = np.radians(theta_deg)
    errors = grouped['error'].values

    # Convert to Cartesian using r_plot
    x = r_plot * np.cos(theta)
    y = r_plot * np.sin(theta)

    # Create a coarse grid first using linear interpolation
    coarse_res = 80
    grid_x_coarse, grid_y_coarse = np.meshgrid(
        np.linspace(x.min(), x.max(), coarse_res),
        np.linspace(y.min(), y.max(), coarse_res)
    )

    # Transform errors to log space for positivity
    #transformed_errors = np.log(errors)
    transformed_errors = inverse_softplus(errors)
    
    coarse_grid = griddata(
        points=(x, y),
        values=transformed_errors,
        xi=(grid_x_coarse, grid_y_coarse),
        method='linear',
        fill_value=np.nan
    )

    # Mask NaNs for spline fitting
    mask = ~np.isnan(coarse_grid)
    xs = np.linspace(x.min(), x.max(), coarse_res)
    ys = np.linspace(y.min(), y.max(), coarse_res)

    # Fill NaNs for spline (optional: nearest)
    from scipy.interpolate import NearestNDInterpolator
    nearest = NearestNDInterpolator(list(zip(grid_x_coarse[mask], grid_y_coarse[mask])),
                                    coarse_grid[mask])
    coarse_grid_filled = coarse_grid.copy()
    coarse_grid_filled[~mask] = nearest(grid_x_coarse[~mask], grid_y_coarse[~mask])

    # Bicubic upsampling
    spline = RectBivariateSpline(ys, xs, coarse_grid_filled, kx=3, ky=3)
    fine_res = 300
    xs_fine = np.linspace(x.min(), x.max(), fine_res)
    ys_fine = np.linspace(y.min(), y.max(), fine_res)
    error_grid_transformed = spline(ys_fine, xs_fine)

    # Inverse transform back to positive space
    #error_grid = np.exp(error_grid_transformed)
    error_grid = softplus(error_grid_transformed)
    
    # Convert grid to polar for plotting
    grid_x, grid_y = np.meshgrid(xs_fine, ys_fine)
    r_grid = np.sqrt(grid_x**2 + grid_y**2)
    theta_grid = np.arctan2(grid_y, grid_x)

    # Max plotting radius is horizon (r_max - min_elevation)
    r_outer = r_max - r.min()

    # Mask values outside the circle (beyond horizon)
    mask_circle = r_grid <= r_outer
    error_grid[~mask_circle] = np.nan
    
    # Plot
    fig, ax = plt.subplots(subplot_kw=dict(projection='polar'), figsize=(8, 8))
    cmap_plot = ax.pcolormesh(theta_grid, r_grid, error_grid, cmap='RdYlBu_r', shading='auto')
    ax.set_ylim(0, r_outer)  # stops plot at the horizon

    # Scatter points as open circles
    ax.scatter(theta, r_plot, facecolors='none', edgecolors='k', s=10, alpha=0.5, label='Data Points')

    ax.set_theta_zero_location('N')
    ax.set_theta_direction(-1)
    r_ticks = np.linspace(0, r_outer, 5)
    ax.set_yticks(r_ticks)
    ax.set_yticklabels([f"{r_max - rt:.0f}°" for rt in r_ticks])
    cbar = plt.colorbar(cmap_plot, ax=ax, label='Sun azimuth estimate - Error (deg)')
    ax.legend(loc='lower right')
    
    plt.show()

if __name__ == "__main__":
    main()
