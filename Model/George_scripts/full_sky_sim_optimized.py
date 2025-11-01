import numpy as np
from PIL import Image
import os
import argparse
import matplotlib.colors as mcolors


# ------------------------------------------------------------
# Coordinate transforms
# ------------------------------------------------------------
def spherical_to_cartesian(radius, azimuth_deg, elevation_deg):
    """Convert spherical (azimuth, elevation) to approximate cartesian pixel coordinates."""
    s = radius * elevation_deg / 90  # distance from image edge (simple projection)
    azimuth_rad = np.radians(azimuth_deg)
    x = (radius - s) * np.sin(azimuth_rad)
    y = radius - (radius - s) * np.cos(azimuth_rad)
    return int(x), int(y)


def cartesian_to_spherical_grid(width, height, center_x, center_y):
    """Vectorized conversion from cartesian pixel coordinates to spherical (azimuth, elevation)."""
    yy, xx = np.indices((height, width))
    azimuth_rad = np.arctan2(xx - center_x, center_y - yy)
    azimuth_deg = (np.degrees(azimuth_rad) + 360) % 360
    radius = np.sqrt((xx - center_x) ** 2 + (yy - center_y) ** 2)
    elevation_deg = np.clip(90 - 90 * radius / center_y, 0, 90)
    return azimuth_deg, elevation_deg


# ------------------------------------------------------------
# Physics: Rayleigh polarization model
# ------------------------------------------------------------
def compute_stokes_field(azimuth_deg, elevation_deg, sun_azimuth_deg, sun_elevation_deg):
    """Compute Stokes parameters (S0, S1, S2), AoLP, and DoP for all pixels."""
    # Convert to radians
    az1 = np.radians(azimuth_deg)
    el1 = np.radians(elevation_deg)
    az2 = np.radians(sun_azimuth_deg)
    el2 = np.radians(sun_elevation_deg)

    # Sun vector
    Sx = np.cos(el2) * np.cos(az2)
    Sy = np.cos(el2) * np.sin(az2)
    Sz = np.sin(el2)

    # Observation vectors
    Ox = np.cos(el1) * np.cos(az1)
    Oy = np.cos(el1) * np.sin(az1)
    Oz = np.sin(el1)

    # Cross product for AoLP
    Nx = Sy * Oz - Sz * Oy
    Ny = Sz * Ox - Sx * Oz
    aolp = np.mod(-np.arctan2(Ny, Nx) + np.pi / 2, np.pi)  # radians, wrapped [0, π)

    # Scattering angle
    dot = Sx * Ox + Sy * Oy + Sz * Oz
    dot = np.clip(dot, -1.0, 1.0)
    theta = np.arccos(dot)

    # Degree of polarization (DoP)
    dop = (np.sin(theta) ** 2) / (1 + np.cos(theta) ** 2)
    dop = np.clip(dop, 0, 1)

    # Intensity (S0)
    S0 = 1.0 * (1 + np.cos(theta) ** 2)
    # Optional horizon darkening: S0 *= np.cos(el1) ** 1.5

    # Stokes parameters
    S1 = S0 * dop * np.cos(2 * aolp)
    S2 = S0 * dop * np.sin(2 * aolp)

    return aolp, dop, S0, S1, S2


# ------------------------------------------------------------
# Image saving functions
# ------------------------------------------------------------
def save_stokes_outputs(aolp, dop, S0, S1, S2, output_txt, out_aolp_img, out_dop_img, out_intensity_img):
    """Save data to text and image files."""
    height, width = S0.shape

    # Save text file
    with open(output_txt, "w") as f:
        f.write("x\ty\taolp\tdop\tS0\tS1\tS2\n")
        for y in range(height):
            for x in range(width):
                f.write(f"{x}\t{y}\t{aolp[y,x]:.4f}\t{dop[y,x]:.4f}\t{S0[y,x]:.4f}\t{S1[y,x]:.4f}\t{S2[y,x]:.4f}\n")

    # AoLP hue image
    hsv = np.zeros((height, width, 3))
    hsv[..., 0] = (aolp % np.pi) / np.pi  # hue
    hsv[..., 1] = 1.0
    hsv[..., 2] = 1.0
    aolp_rgb = mcolors.hsv_to_rgb(hsv)
    Image.fromarray((aolp_rgb * 255).astype(np.uint8)).save(out_aolp_img)

    # DoP grayscale
    dop_img = np.clip(np.nan_to_num(dop), 0, 1)
    Image.fromarray((dop_img * 255).astype(np.uint8)).save(out_dop_img)

    # Intensity grayscale (normalized)
    S0_norm = (S0 - S0.min()) / (S0.max() - S0.min())
    Image.fromarray((S0_norm * 255).astype(np.uint8)).save(out_intensity_img)


def save_polarizer_npy(S0, S1, S2, folder_path):
    """Save simulated polarizer intensities at 0°, 45°, 90°, 135° as .npy files."""
    os.makedirs(folder_path, exist_ok=True)
    for angle_deg in [0, 45, 90, 135]:
        angle_rad = np.radians(angle_deg)
        I_phi = 0.5 * (S0 + S1 * np.cos(2 * angle_rad) + S2 * np.sin(2 * angle_rad))
        np.save(os.path.join(folder_path, f"img_{angle_deg}.npy"), I_phi)


# ------------------------------------------------------------
# Main program
# ------------------------------------------------------------
def main():
    parser = argparse.ArgumentParser(description="Simulate Rayleigh-sky polarization field.")
    parser.add_argument("input_image", help="Input image (used only for size)")
    parser.add_argument("output_txt", help="Output text file with Stokes parameters")
    parser.add_argument("output_aolp_image", help="Output AoLP (hue) image")
    parser.add_argument("output_dop_image", help="Output DoP (grayscale) image")
    parser.add_argument("output_intensity_image", help="Output intensity (S0) image")
    parser.add_argument("output_npy_folder", help="Folder to save polarizer .npy files")
    parser.add_argument("sun_azimuth", type=float, help="Sun azimuth angle in degrees")
    parser.add_argument("sun_elevation", type=float, help="Sun elevation angle in degrees")
    args = parser.parse_args()

    # Load image for size
    image = Image.open(args.input_image)
    width, height = image.size
    center_x, center_y = width // 2, height // 2

    # Compute spherical coordinates grid
    azimuth_deg, elevation_deg = cartesian_to_spherical_grid(width, height, center_x, center_y)

    # Compute Stokes field
    aolp, dop, S0, S1, S2 = compute_stokes_field(
        azimuth_deg, elevation_deg, -args.sun_azimuth, args.sun_elevation
    )

    # Save all outputs
    save_stokes_outputs(aolp, dop, S0, S1, S2,
                        args.output_txt,
                        args.output_aolp_image,
                        args.output_dop_image,
                        args.output_intensity_image)
    save_polarizer_npy(S0, S1, S2, args.output_npy_folder)


if __name__ == "__main__":
    main()
