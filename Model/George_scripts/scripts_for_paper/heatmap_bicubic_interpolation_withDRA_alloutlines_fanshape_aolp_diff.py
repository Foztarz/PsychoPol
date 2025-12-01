import sys
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from scipy.interpolate import griddata, RectBivariateSpline, NearestNDInterpolator
from shapely.geometry import Point
from shapely.ops import unary_union


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

    # --- Load main data ---
    # Expected columns: sun_elevation, sun_azimuth, fanshape_aolp_diff, error
    df = pd.read_csv(data_filepath, sep="\t", header=0)
    grouped = df.groupby(['sun_elevation', 'sun_azimuth'], as_index=False).mean()

    # Data to plot
    r = grouped['sun_elevation'].values
    r_plot = r_max - r
    theta_deg = grouped['sun_azimuth'].values
    theta = np.radians(theta_deg)

    # Use the 3rd column: fanshape_aolp_diff (radians → degrees), absolute value
    fanshape_aolp_diff_rad = grouped['fanshape_aolp_diff'].values
    fanshape_aolp_diff_deg = np.degrees(fanshape_aolp_diff_rad)

    # Convert to Cartesian coordinates
    x = r_plot * np.cos(theta)
    y = r_plot * np.sin(theta)

    # --- Interpolation ---
    coarse_res = 80
    grid_x_coarse, grid_y_coarse = np.meshgrid(
        np.linspace(x.min(), x.max(), coarse_res),
        np.linspace(y.min(), y.max(), coarse_res)
    )

    coarse_grid = griddata(
        points=(x, y),
        values=fanshape_aolp_diff_deg,
        xi=(grid_x_coarse, grid_y_coarse),
        method='linear',
        fill_value=np.nan
    )

    mask = ~np.isnan(coarse_grid)
    xs = np.linspace(x.min(), x.max(), coarse_res)
    ys = np.linspace(y.min(), y.max(), coarse_res)
    nearest = NearestNDInterpolator(list(zip(grid_x_coarse[mask], grid_y_coarse[mask])),
                                    coarse_grid[mask])
    coarse_grid_filled = coarse_grid.copy()
    coarse_grid_filled[~mask] = nearest(grid_x_coarse[~mask], grid_y_coarse[~mask])

    # Bicubic upsampling
    spline = RectBivariateSpline(ys, xs, coarse_grid_filled, kx=3, ky=3)
    fine_res = 600
    xs_fine = np.linspace(x.min(), x.max(), fine_res)
    ys_fine = np.linspace(y.min(), y.max(), fine_res)
    value_grid = spline(ys_fine, xs_fine)

    # --- Mask outside circular region ---
    grid_x, grid_y = np.meshgrid(xs_fine, ys_fine)
    r_grid = np.sqrt(grid_x**2 + grid_y**2)
    r_outer = r_max - r.min()
    mask_circle = r_grid <= r_outer
    value_grid[~mask_circle] = np.nan

    # --- Plot ---
    fig, ax = plt.subplots(subplot_kw=dict(projection='polar'), figsize=(8, 8))
    cmap = plt.cm.PiYG  # perceptual colormap (low = good, high = bad)
    cmap.set_bad((0, 0, 0, 0))  # fully transparent for NaN

    # Convert grid to polar coordinates for pcolormesh
    xs_edges = np.linspace(x.min(), x.max(), fine_res + 1)
    ys_edges = np.linspace(y.min(), y.max(), fine_res + 1)
    grid_x_edges, grid_y_edges = np.meshgrid(xs_edges, ys_edges)
    r_edges = np.sqrt(grid_x_edges**2 + grid_y_edges**2)
    theta_edges = np.unwrap(np.arctan2(grid_y_edges, grid_x_edges), axis=1)

    # Normalize theta range
    theta_min = theta_edges.min()
    if theta_min < 0:
        theta_edges = theta_edges + (np.ceil(-theta_min / (2 * np.pi)) * 2 * np.pi)

    Z = np.ma.masked_invalid(value_grid)
    cmap_plot = ax.pcolormesh(theta_edges, r_edges, Z, cmap=cmap,
                              shading='auto', vmin=-45, vmax=45,
                              antialiased=False, linewidth=0)

    ax.set_ylim(0, r_outer)

    # Scatter original data points
    ax.scatter(theta, r_plot, facecolors='none', edgecolors='k',
               s=10, alpha=0.6, label='Data Points', linewidth=0.5)

    # --- FOV outlines ---
    if union_shape.geom_type == "Polygon":
        polys = [union_shape]
    elif union_shape.geom_type == "MultiPolygon":
        polys = list(union_shape.geoms)
    else:
        polys = []

    outline_plotted = False
    for poly in polys:
        outline_x, outline_y = poly.exterior.xy
        outline_r = np.sqrt(np.array(outline_x)**2 + np.array(outline_y)**2)
        outline_theta = np.unwrap(np.arctan2(outline_y, outline_x))

        label = 'DRA outline' if not outline_plotted else None
        ax.plot(outline_theta, outline_r, color='red', linewidth=2.5, alpha=0.9, label=label)

        outline_theta_mirror = np.unwrap(-np.arctan2(outline_y, outline_x))
        ax.plot(outline_theta_mirror, outline_r, color='red', linewidth=2.5, alpha=0.9)

        outline_plotted = True

    # --- Formatting ---
    ax.set_theta_zero_location('N')
    ax.set_theta_direction(-1)
    r_ticks = np.linspace(0, r_outer, 5)
    ax.set_yticks(r_ticks)
    ax.set_yticklabels([f"{r_max - rt:.0f}°" for rt in r_ticks])
    plt.colorbar(cmap_plot, ax=ax, label='fanshape_aolp_diff (degrees)')
    ax.legend(loc='lower right')

    # --- Clean version (for saving) ---
    fig2, ax2 = plt.subplots(subplot_kw=dict(projection='polar'), figsize=(8, 8))
    Z = np.ma.masked_invalid(value_grid)
    cmap.set_bad((0, 0, 0, 0))
    ax2.pcolormesh(theta_edges, r_edges, Z,
                   cmap=cmap,
                   shading='auto',
                   vmin=-45, vmax=45,
                   antialiased=False,
                   linewidth=0)

    ax2.scatter(theta, r_plot, facecolors='none', edgecolors='k',
                s=10, alpha=0.6, linewidth=0.5)

    for poly in polys:
        outline_x, outline_y = poly.exterior.xy
        outline_r = np.sqrt(np.array(outline_x)**2 + np.array(outline_y)**2)
        outline_theta = np.unwrap(np.arctan2(outline_y, outline_x))
        ax2.plot(outline_theta, outline_r, color='red', linewidth=2.5, alpha=0.9)
        outline_theta_mirror = np.unwrap(-np.arctan2(outline_y, outline_x))
        ax2.plot(outline_theta_mirror, outline_r, color='red', linewidth=2.5, alpha=0.9)

    ax2.set_theta_zero_location('N')
    ax2.set_theta_direction(-1)
    ax2.set_ylim(0, r_outer)
    ax2.set_xticks([])
    ax2.set_yticks([])

    # Grid overlay for reference
    num_radial = 8
    angles = np.linspace(0, 2*np.pi, num_radial, endpoint=False)
    for angle in angles:
        ax2.plot([angle, angle], [0, r_outer], color='k', linewidth=0.5, alpha=0.5)
    num_circles = 4
    radii = np.linspace(r_outer/num_circles, r_outer, num_circles)
    theta_full = np.linspace(0, 2*np.pi, 500)
    for radius in radii:
        ax2.plot(theta_full, np.full_like(theta_full, radius), color='k', linewidth=0.5, alpha=0.5)

    fig2.savefig("fanshape_aolp_diff_heatmap.png", dpi=300, bbox_inches='tight', transparent=False)
    plt.close(fig2)
    plt.show()


if __name__ == "__main__":
    main()
