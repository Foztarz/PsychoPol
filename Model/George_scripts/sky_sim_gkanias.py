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

def tilt(theta_t, phi_t, theta, phi):
    """
    Rotate/tilt spherical points (theta,phi) by tilt (theta_t,phi_t).
    """
    # note: theta, phi may be numpy arrays
    st = np.sin(theta)
    ct = np.cos(theta)
    stt = np.sin(theta_t)
    ctt = np.cos(theta_t)
    shtt2 = np.square(np.sin(theta_t / 2.))
    # phi, phi_t arrays/broadcast safe
    x = st * (np.sin(phi) - 2 * shtt2 * np.sin(phi_t) * np.cos(phi_t - phi)) + stt * ct * np.sin(phi_t)

    y = -st * np.sin(phi) * shtt2 * np.sin(2 * phi_t) + \
        st * np.cos(phi) * (ctt * np.square(np.cos(phi_t)) + np.square(np.sin(phi_t))) + \
        ct * stt * np.cos(phi_t)

    z = ctt * ct - stt * st * np.cos(phi_t - phi)

    # arccos may produce NaN if numerical error pushes z slightly outside [-1,1]
    z = np.clip(z, -1.0, 1.0)
    e = np.arccos(z)
    a = np.arctan2(x, y)   # returns angle in (-pi, pi]
    return e, a

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


def generate_clear_sky_image(resolution, sun_az_deg, sun_el_deg, output_folder="sky_output"):
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
    DOP_img[mask] = DOP  # <-- this makes DOP_img ready for visualization
    # angle of polarization
    _, AoLP_masked = tilt(theta_s, phi_s + np.pi, theta, phi)

    # shift by 90 and negate to match our images
    AoLP_masked = -np.mod(AoLP_masked+np.pi/2, np.pi)  # (0, π)

    # fill AoLP full image (and optionally AoLP_img for inspection)
    AoLP_img = np.full_like(Y_img, np.nan)
    AoLP_img[mask] = AoLP_masked
    
    # Stokes parameters
    S0_masked = Y
    S1_masked = Y * DOP * np.cos(2*AoLP_masked)
    S2_masked = Y * DOP * np.sin(2*AoLP_masked)

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

    # reference system change here because the 'polarizers' were shifted by 90deg compared to the images
    os.makedirs(output_folder, exist_ok=True)
    np.save(f"{output_folder}/S0.npy", S0)
    np.save(f"{output_folder}/img_000.npy", I90)
    np.save(f"{output_folder}/img_045.npy", I135)
    np.save(f"{output_folder}/img_090.npy", I0)
    np.save(f"{output_folder}/img_135.npy", I45)

    # Optional: save Y as PNG
    from PIL import Image
    S0_uint8 = ((S0 - S0.min()) / (S0.max() - S0.min()) * 255).astype(np.uint8)
    Image.fromarray(S0_uint8).save(f"{output_folder}/S0.png")

    return Y_img, DOP_img, AoLP_img, S0, S1, S2, I0, I45, I90, I135



if __name__ == "__main__":
    import matplotlib.pyplot as plt
    import numpy as np

    # Example usage
    resolution = int(sys.argv[4])
    sun_azimuth = -int(sys.argv[1])  # degrees, 0 = up, counterclockwise
    sun_elevation = int(sys.argv[2])  # degrees, 0 = horizon, 90 = zenith
    output_folder = sys.argv[3] if len(sys.argv) > 3 else "sky_output"
    
    # Generate sky arrays and capture AoLP
    Y_img, DOP_img, AoLP_img, S0, S1, S2, I0, I45, I90, I135 = generate_clear_sky_image(
        resolution, sun_azimuth, sun_elevation, output_folder=output_folder
    )

    # --- Visualization of AoP ---
    AoLP_deg = np.degrees(AoLP_img)  # convert radians to degrees
    mask = ~np.isnan(AoLP_deg)
    AoLP_plot = np.zeros_like(AoLP_deg)
    AoLP_plot[mask] = AoLP_deg[mask]

    plt.figure(figsize=(6,6))
    plt.imshow(AoLP_plot, cmap='hsv', origin='upper')  # cyclic colormap
    plt.colorbar(label="Angle of Polarization (deg)")
    plt.title("Sky Angle of Polarization (AoP)")
    plt.axis('off')
    plt.show()
    
    plt.figure(figsize=(6,6))
    plt.imshow(DOP_img, cmap='viridis', origin='upper', vmin=0, vmax=1)
    plt.colorbar(label="Degree of Linear Polarization (DoLP)")
    plt.title("Sky Degree of Linear Polarization (DoLP)")
    plt.axis('off')
    plt.show()

    print(f"Sky arrays saved in ./{output_folder}/")

