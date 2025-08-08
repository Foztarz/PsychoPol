import sys
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from scipy.interpolate import griddata, RectBivariateSpline
from shapely.geometry import Point
from shapely.ops import unary_union

def inverse_softplus(y):
    return np.log(np.exp(y) - 1)

def softplus(x):
    return np.log1p(np.exp(x))

def main():
    if len(sys.argv) != 3:
        print("Usage: python heatmap.py <data.tsv> <fov.tsv>")
        sys.exit(1)

    data_filepath = sys.argv[1]
    fov_filepath = sys.argv[2]

    r_max = 90  # zenith = 90°

    # --- Load and process FOV file ---
    df_fov = pd.read_csv(fov_filepath, sep="\t", header=None, names=["sun_azimuth", "sun_elevation"])
    r_fov = df_fov['sun_elevation'].values
    r_plot_fov = r_max - r_fov
    theta_deg_fov = df_fov['sun_azimuth'].values
    theta_fov = np.radians(theta_deg_fov)
    x_fov = r_plot_fov * np.cos(theta_fov)
    y_fov = r_plot_fov * np.sin(theta_fov)

    circle_radius = 3.12  # degrees of FWHM/2
    shapes = [Point(x, y).buffer(circle_radius) for x, y in zip(x_fov, y_fov)]
    union_shape = unary_union(shapes)
    outline_x, outline_y = union_shape.exterior.xy
    outline_r = np.sqrt(np.array(outline_x)**2 + np.array(outline_y)**2)
    outline_theta = np.arctan2(outline_y, outline_x)

    # --- Load main data ---
    df = pd.read_csv(data_filepath, sep="\t", header=0)
    grouped = df.groupby(['sun_elevation', 'sun_azimuth'], as_index=False).mean()

    r = grouped['sun_elevation'].values
    r_plot = r_max - r
    theta_deg = grouped['sun_azimuth'].values
    theta = np.radians(theta_deg)
    errors = grouped['error'].values

    # Convert to Cartesian using r_plot
    x = r_plot * np.cos(theta)
    y = r_plot * np.sin(theta)

    # Coarse grid interpolation
    coarse_res = 80
    grid_x_coarse, grid_y_coarse = np.meshgrid(
        np.linspace(x.min(), x.max(), coarse_res),
        np.linspace(y.min(), y.max(), coarse_res)
    )
    transformed_errors = inverse_softplus(errors)
    coarse_grid = griddata(
        points=(x, y),
        values=transformed_errors,
        xi=(grid_x_coarse, grid_y_coarse),
        method='linear',
        fill_value=np.nan
    )

    mask = ~np.isnan(coarse_grid)
    xs = np.linspace(x.min(), x.max(), coarse_res)
    ys = np.linspace(y.min(), y.max(), coarse_res)
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

    error_grid = softplus(error_grid_transformed)

    # Convert grid to polar for plotting
    grid_x, grid_y = np.meshgrid(xs_fine, ys_fine)
    r_grid = np.sqrt(grid_x**2 + grid_y**2)
    theta_grid = np.arctan2(grid_y, grid_x)

    r_outer = r_max - r.min()
    mask_circle = r_grid <= r_outer
    error_grid[~mask_circle] = np.nan

    # --- Plot ---
    fig, ax = plt.subplots(subplot_kw=dict(projection='polar'), figsize=(8, 8))
    cmap_plot = ax.pcolormesh(theta_grid, r_grid, error_grid, cmap='RdYlBu_r', shading='auto', vmin=0, vmax=180)
    ax.set_ylim(0, r_outer)

    # Scatter points
    ax.scatter(theta, r_plot, facecolors='none', edgecolors='k', s=10, alpha=0.5, label='Data Points')

    # FOV outline
    ax.plot(outline_theta, outline_r, color='k', linewidth=1.5, alpha=0.8, label='DRA outline')

    # Mirror outline across center vertical (θ=0)
    outline_theta_mirror = -outline_theta
    # Wrap angles back to [-pi, pi]
    outline_theta_mirror = (outline_theta_mirror + np.pi) % (2 * np.pi) - np.pi
    ax.plot(outline_theta_mirror, outline_r, color='k', linewidth=1.5, alpha=0.8)


    # Formatting
    ax.set_theta_zero_location('N')
    ax.set_theta_direction(-1)
    r_ticks = np.linspace(0, r_outer, 5)
    ax.set_yticks(r_ticks)
    ax.set_yticklabels([f"{r_max - rt:.0f}°" for rt in r_ticks])
    plt.colorbar(cmap_plot, ax=ax, label='Sun azimuth estimate - Error (deg)')
    ax.legend(loc='lower right')

    plt.show()


if __name__ == "__main__":
    main()
