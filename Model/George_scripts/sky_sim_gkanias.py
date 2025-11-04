#!/usr/bin/env python
import numpy as np
import os
import sys
from PIL import Image

# --- Sky model coefficients (from original Sky class) ---
T_L = np.array([[ 0.1787, -1.4630],
                [-0.3554,  0.4275],
                [-0.0227,  5.3251],
                [ 0.1206, -2.5771],
                [-0.0670,  0.3703]])

C1 = 0.6
C2 = 4.0
TAU_L_DEFAULT = 2.0

def update_luminance_coefficients(tau_L):
    a, b, c, d, e = T_L.dot(np.array([tau_L, 1.]))
    return a, b, c, d, e

def Y_z(theta_s, tau_L=TAU_L_DEFAULT):
    chi = (4. / 9. - tau_L / 120.) * (np.pi - 2 * theta_s)
    return (4.0453 * tau_L - 4.9710) * np.tan(chi) - 0.2155 * tau_L + 2.4192

def M_p(tau_L=TAU_L_DEFAULT):
    return np.exp(-(tau_L - C1) / (C2 + 1e-12))

def Luminance(chi, z, a, b, c, d, e):
    z = np.array(z)
    i = z < (np.pi/2)
    f = np.zeros_like(z)
    if z.ndim > 0:
        f[i] = (1. + a * np.exp(b / (np.cos(z[i]) + 1e-12)))
    elif i:
        f = (1. + a * np.exp(b / (np.cos(z) + 1e-12)))
    phi = (1. + c * np.exp(d * chi) + e * np.square(np.cos(chi)))
    return f * phi

def spherical_to_cartesian(radius, azimuth_deg, elevation_deg):
    s = radius * elevation_deg / 90  # distance from circle's circumference
    azimuth_rad = np.radians(azimuth_deg)
    elevation_rad = np.radians(elevation_deg)
    x = int((radius - s) * np.sin(azimuth_rad) + radius)
    y = int(radius - (radius - s) * np.cos(azimuth_rad))
    return x, y

def cartesian_to_spherical(x, y, radius):
    """
    Inverse of spherical_to_cartesian().
    Returns azimuth [deg 0-360] and elevation [deg 0-90] for pixel (x,y)
    """
    dx = x - radius
    dy = radius - y
    r = np.sqrt(dx**2 + dy**2)
    if r > radius:
        return None, None  # outside fisheye
    el = 90 * (1 - r / radius)
    az = (np.degrees(np.arctan2(dx, dy))) % 360
    return az, el

def generate_clear_sky_image(resolution, sun_az_deg, sun_el_deg):
    import numpy as np
    import os
    from PIL import Image

    radius = resolution // 2

    # Prepare output arrays
    Y_img = np.zeros((resolution, resolution))
    DOP_img = np.zeros_like(Y_img)
    AoLP_img = np.zeros_like(Y_img)
    S0 = np.zeros_like(Y_img)
    S1 = np.zeros_like(Y_img)
    S2 = np.zeros_like(Y_img)
    I0 = np.zeros_like(Y_img)
    I45 = np.zeros_like(Y_img)
    I90 = np.zeros_like(Y_img)
    I135 = np.zeros_like(Y_img)

    # Sky model
    a, b, c, d, e = update_luminance_coefficients(TAU_L_DEFAULT)
    theta_s = np.radians(90 - sun_el_deg)
    phi_s = np.radians(sun_az_deg)

    # Create meshgrid of pixel coordinates
    i = np.arange(resolution)
    j = np.arange(resolution)
    jj, ii = np.meshgrid(j, i)

    # Compute dx, dy relative to center
    dx = jj - radius
    dy = radius - ii
    r = np.sqrt(dx**2 + dy**2)

    # Mask for pixels inside the fisheye circle
    mask = r <= radius

    # Compute azimuth and elevation
    az = (np.degrees(np.arctan2(dx, dy))) % 360
    el = 90 * (1 - r / radius)

    # Apply mask
    az_masked = az[mask]
    el_masked = el[mask]

    theta = np.radians(90 - el_masked)
    phi = np.radians(az_masked)

    # gamma angle between sun and sky element
    gamma = np.arccos(np.cos(theta) * np.cos(theta_s) +
                      np.sin(theta) * np.sin(theta_s) * np.cos(phi - phi_s))

    # luminance
    i_prez = Luminance(gamma, theta, a, b, c, d, e)
    i_00 = Luminance(0., theta_s, a, b, c, d, e)
    Y = np.maximum(Y_z(theta_s) * i_prez / (i_00 + 1e-12), 0.)

    # degree of polarization
    lp = np.square(np.sin(gamma)) / (1 + np.square(np.cos(gamma)))
    DOP = np.clip(2./np.pi * M_p() * lp * (theta * np.cos(theta) + (np.pi/2 - theta) * i_prez), 0., 1.)

    # angle of polarization
    AoLP = (phi_s + np.pi) % (2*np.pi)

    # Stokes parameters
    S0_masked = Y
    S1_masked = Y * DOP * np.cos(2*AoLP)
    S2_masked = Y * DOP * np.sin(2*AoLP)

    # Simulated polarizers
    I0_masked = 0.5 * (S0_masked + S1_masked)
    I45_masked = 0.5 * (S0_masked + S2_masked)
    I90_masked = 0.5 * (S0_masked - S1_masked)
    I135_masked = 0.5 * (S0_masked - S2_masked)

    # Fill the full images
    S0[mask] = S0_masked

    I0[mask] = I0_masked
    I45[mask] = I45_masked
    I90[mask] = I90_masked
    I135[mask] = I135_masked

    # Save arrays
    os.makedirs("sky_output", exist_ok=True)

    np.save("sky_output/S0.npy", S0)
    # reference system change below (by 90deg to match images)
    np.save("sky_output/img_000.npy", I90)
    np.save("sky_output/img_045.npy", I135)
    np.save("sky_output/img_090.npy", I0)
    np.save("sky_output/img_135.npy", I45)

    # Optional: save Y as PNG
    from PIL import Image
    S0_uint8 = ((S0 - S0.min()) / (S0.max() - S0.min()) * 255).astype(np.uint8)
    Image.fromarray(S0_uint8).save("sky_output/S0.png")

    return Y_img, DOP_img, AoLP_img, S0, S1, S2, I0, I45, I90, I135



if __name__ == "__main__":
    # Example usage
    resolution = 1683
    sun_azimuth = -int(sys.argv[1])  # degrees, 0 = up, counterclockwise
    sun_elevation = int(sys.argv[2])  # degrees, 0 = horizon, 90 = zenith
    
    generate_clear_sky_image(resolution, sun_azimuth, sun_elevation)
    print("Sky arrays saved in ./sky_output/")
