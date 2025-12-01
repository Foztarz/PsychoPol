import sys
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from scipy.interpolate import griddata, RectBivariateSpline
from shapely.geometry import Point
from shapely.ops import unary_union
import matplotlib.colors as mcolors
from scipy.interpolate import NearestNDInterpolator


def custom_seismic_like():
    """
    Diverging colormap:
    dark blue → light blue → near white → white → near white → light red → dark red
    """
    cdict = [
        (0.0,   "#041C3A"),  # very dark navy blue
        (0.15,  "#084594"),  # strong deep blue
        (0.30,  "#C6DBEF"),  # pale blue (fade out earlier)
        (0.45,  "#F2F2F2"),  # almost white (start wide white band)
        (0.55,  "#FFFFFF"),  # pure white (center)
        (0.65,  "#F2F2F2"),  # almost white (end wide white band)
        (0.80,  "#FBB4AE"),  # pale red (start reds later)
        (0.90,  "#B30000"),  # strong deep red
        (1.0,   "#4A0000")   # very dark crimson
    ]
    return mcolors.LinearSegmentedColormap.from_list("custom_blue_white_red", cdict, N=256)


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

    # --- Load main data ---
    df = pd.read_csv(data_filepath, sep="\t", header=0)
    grouped = df.groupby(['sun_elevation', 'sun_azimuth'], as_index=False).mean()

    r = grouped['sun_elevation'].values
    r_plot = r_max - r
    theta_deg = grouped['sun_azimuth'].values
    theta = np.radians(theta_deg)
    PRC = grouped['PRC'].values

    # Convert to Cartesian using r_plot
    x = r_plot * np.cos(theta)
    y = r_plot * np.sin(theta)

    # Coarse grid interpolation
    coarse_res = 80
    grid_x_coarse, grid_y_coarse = np.meshgrid(
        np.linspace(x.min(), x.max(), coarse_res),
        np.linspace(y.min(), y.max(), coarse_res)
    )
    coarse_grid = griddata(
        points=(x, y),
        values=-PRC, ## negative because phi max 2 is driving negative PRC in the script, we want to plot it as positive
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
    PRC_grid_transformed = spline(ys_fine, xs_fine)

    PRC_grid = PRC_grid_transformed

    # Convert grid to polar for masking
    grid_x, grid_y = np.meshgrid(xs_fine, ys_fine)
    r_grid = np.sqrt(grid_x**2 + grid_y**2)
    theta_grid = np.arctan2(grid_y, grid_x)

    r_outer = r_max - r.min()
    mask_circle = r_grid <= r_outer
    PRC_grid[~mask_circle] = np.nan

    
    # --- Plot ---
    fig, ax = plt.subplots(subplot_kw=dict(projection='polar'), figsize=(8, 8))
    cmap_custom = custom_seismic_like()

    # Ensure masked values are fully transparent (avoid white NaN lines)
    cmap_custom.set_bad((0, 0, 0, 0))  # RGBA with 0 alpha -> fully transparent

    # Masked array for pcolormesh
    Z = np.ma.masked_invalid(PRC_grid)

    # --- IMPORTANT: build vertex grids (edges) for pcolormesh instead of using cell centers ---
    # pcolormesh expects (nr+1, nt+1) shaped vertex arrays when Z has shape (nr, nt). Building
    # x/y edges and converting them to (r,theta) vertices avoids topology seams and discontinuities
    # caused by angle wrapping at -pi/pi.
    xs_edges = np.linspace(x.min(), x.max(), fine_res + 1)
    ys_edges = np.linspace(y.min(), y.max(), fine_res + 1)
    grid_x_edges, grid_y_edges = np.meshgrid(xs_edges, ys_edges)

    r_edges = np.sqrt(grid_x_edges**2 + grid_y_edges**2)
    theta_edges = np.arctan2(grid_y_edges, grid_x_edges)

    # Unwrap along the azimuthal (column) direction to remove the -pi/pi jump. This preserves
    # continuity across the whole grid so pcolormesh doesn't draw a seam.
    theta_edges = np.unwrap(theta_edges, axis=1)

    # OPTIONAL: shift theta so minimum is >= 0 — not strictly necessary but helps keeping values
    # in a conventional range for the polar axis. We only add multiples of 2*pi, so continuity
    # is preserved.
    theta_min = theta_edges.min()
    if theta_min < 0:
        theta_edges = theta_edges + (np.ceil(-theta_min / (2 * np.pi)) * 2 * np.pi)

    # Now use the vertex arrays with pcolormesh. Turn off antialiasing and edge lines to avoid
    # thin white borders between polygons.
    cmap_plot = ax.pcolormesh(theta_edges, r_edges, Z, cmap="plasma",
                              shading='auto',
                              antialiased=False, linewidth=0)

    ax.set_ylim(0, r_outer)

    # Scatter points (data centers)
    ax.scatter(theta, r_plot, facecolors='none', edgecolors='k',
               s=10, alpha=0.6, label='Data Points', linewidth=0.5)

    # --- Plot all FOV outlines ---
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
        outline_theta = np.arctan2(outline_y, outline_x)

        # Convert outline_theta to continuous values consistent with theta_edges by unwrapping
        # along the sequence so the outline doesn't get cut at the -pi/pi boundary.
        outline_theta = np.unwrap(outline_theta)

        # Original outline
        label = 'DRA outline' if not outline_plotted else None
        ax.plot(outline_theta, outline_r, color='grey', linewidth=2.5, alpha=0.9, label=label)

        # Mirrored outline
        outline_theta_mirror = -outline_theta
        outline_theta_mirror = np.unwrap(outline_theta_mirror)
        ax.plot(outline_theta_mirror, outline_r, color='grey', linewidth=2.5, alpha=0.9)

        outline_plotted = True

    # Formatting
    ax.set_theta_zero_location('N')
    ax.set_theta_direction(-1)
    r_ticks = np.linspace(0, r_outer, 5)
    ax.set_yticks(r_ticks)
    ax.set_yticklabels([f"{r_max - rt:.0f}°" for rt in r_ticks])
    plt.colorbar(cmap_plot, ax=ax, label='PRC')
    ax.legend(loc='lower right')

    fig2, ax2 = plt.subplots(subplot_kw=dict(projection='polar'), figsize=(8, 8))

    # Plot heatmap
    Z = np.ma.masked_invalid(PRC_grid)
    cmap_custom.set_bad((0, 0, 0, 0))
    ax2.pcolormesh(theta_edges, r_edges, Z,
                   cmap="plasma",
                   shading='auto',
                   antialiased=False,
                   linewidth=0)

    # Scatter points
    ax2.scatter(theta, r_plot, facecolors='none', edgecolors='k',
                s=10, alpha=0.6, linewidth=0.5)

    # FOV outlines
    for poly in polys:
        outline_x, outline_y = poly.exterior.xy
        outline_r = np.sqrt(np.array(outline_x)**2 + np.array(outline_y)**2)
        outline_theta = np.unwrap(np.arctan2(outline_y, outline_x))
        ax2.plot(outline_theta, outline_r, color='grey', linewidth=2.5, alpha=0.9)
        
        outline_theta_mirror = np.unwrap(-np.arctan2(outline_y, outline_x))
        ax2.plot(outline_theta_mirror, outline_r, color='grey', linewidth=2.5, alpha=0.9)

    # Keep the polar axis for correct theta/r plotting
    ax2.set_theta_zero_location('N')
    ax2.set_theta_direction(-1)
    ax2.set_ylim(0, r_outer)

    # Hide default ticks
    ax2.set_xticks([])
    ax2.set_yticks([])

    # Draw radial lines (from center to outer radius)
    num_radial = 8  # number of spokes
    angles = np.linspace(0, 2*np.pi, num_radial, endpoint=False)
    for angle in angles:
        ax2.plot([angle, angle], [0, r_outer], color='k', linewidth=0.5, alpha=0.5)

    # Draw concentric circles
    num_circles = 4
    radii = np.linspace(r_outer/num_circles, r_outer, num_circles)
    theta_full = np.linspace(0, 2*np.pi, 500)
    for radius in radii:
        ax2.plot(theta_full, np.full_like(theta_full, radius), color='k', linewidth=0.5, alpha=0.5)



    # Save figure
    fig2.savefig("heatmap_clean.png", dpi=300, bbox_inches='tight', transparent=False)
    plt.close(fig2)


    plt.show()
    


if __name__ == "__main__":
    main()
