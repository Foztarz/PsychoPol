import numpy as np
import matplotlib.pyplot as plt
from sky import Sky

def create_fisheye_sky_image(image_size=1683, sun_altitude=45, sun_azimuth=135, turbidity=2.0):
    """
    Create a sky image using fisheye projection
    """
    # Convert sun position to radians for Sky class
    sun_theta = np.radians(90 - sun_altitude)  # theta_s is zenith distance
    sun_phi = np.radians(sun_azimuth)
    
    # Create sky instance
    sky = Sky(theta_s=sun_theta, phi_s=sun_phi)
    sky.tau_L = turbidity
    
    # Create output images
    intensity_img = np.zeros((image_size, image_size))
    dop_img = np.zeros((image_size, image_size))
    aolp_img = np.full((image_size, image_size), np.nan)
    
    radius = image_size // 2
    
    # Create pixel grid with origin at center
    x = np.linspace(-radius, radius, image_size)
    y = np.linspace(-radius, radius, image_size)
    xx, yy = np.meshgrid(x, y)
    
    # Calculate distance from center and angle for each pixel
    r = np.sqrt(xx**2 + yy**2)
    phi = np.arctan2(yy, xx)  # azimuth angle (-π to π)
    
    # Fisheye projection: r maps to elevation angle
    # r = 0 at zenith (center), r = radius at horizon (edge)
    valid_mask = r <= radius
    elevation = 90 * (1 - r[valid_mask] / radius)  # 90° at center, 0° at edge
    azimuth = np.degrees(phi[valid_mask]) % 360  # convert to 0-360°
    
    # Convert to spherical coordinates for sky model
    theta = np.radians(90 - elevation)  # zenith distance
    phi_sky = np.radians(azimuth)
    
    # Compute sky parameters for all valid pixels
    Y, DOP, AOP = sky(theta=theta, phi=phi_sky)
    
    # Assign values to output images
    intensity_img[valid_mask] = Y
    dop_img[valid_mask] = DOP
    aolp_img[valid_mask] = AOP
    
    # Set pixels beyond horizon to 0/NaN
    intensity_img[~valid_mask] = 0
    dop_img[~valid_mask] = 0
    aolp_img[~valid_mask] = np.nan
    
    return intensity_img, dop_img, aolp_img

def compute_stokes_parameters(intensity, dop, aolp):
    """Compute Stokes parameters from intensity, DoLP and AoLP"""
    S0 = intensity
    S1 = S0 * dop * np.cos(2 * aolp)
    S2 = S0 * dop * np.sin(2 * aolp)
    return S0, S1, S2

def visualize_results(intensity, dop, aolp, sun_altitude, sun_azimuth):
    """Visualize the results"""
    # Compute Stokes parameters
    S0, S1, S2 = compute_stokes_parameters(intensity, dop, aolp)
    
    # Get image size from the intensity array shape
    image_size = intensity.shape[0]
    
    fig, axes = plt.subplots(2, 3, figsize=(15, 10))
    
    # Intensity (S0)
    im0 = axes[0, 0].imshow(S0, cmap='viridis', origin='upper')
    axes[0, 0].set_title(f'Intensity (S0)\nSun: alt={sun_altitude}°, az={sun_azimuth}°')
    plt.colorbar(im0, ax=axes[0, 0])
    
    # DoLP
    im1 = axes[0, 1].imshow(dop, cmap='hot', vmin=0, vmax=1, origin='upper')
    axes[0, 1].set_title('Degree of Polarization (DoLP)')
    plt.colorbar(im1, ax=axes[0, 1])
    
    # AoLP
    im2 = axes[0, 2].imshow(aolp, cmap='hsv', vmin=-np.pi, vmax=np.pi, origin='upper')
    axes[0, 2].set_title('Angle of Polarization (AoLP)')
    plt.colorbar(im2, ax=axes[0, 2])
    
    # Stokes Q (S1)
    im3 = axes[1, 0].imshow(S1, cmap='coolwarm', origin='upper')
    axes[1, 0].set_title('Stokes Q (S1)')
    plt.colorbar(im3, ax=axes[1, 0])
    
    # Stokes U (S2)
    im4 = axes[1, 1].imshow(S2, cmap='coolwarm', origin='upper')
    axes[1, 1].set_title('Stokes U (S2)')
    plt.colorbar(im4, ax=axes[1, 1])
    
    # Polarization pattern overlay
    axes[1, 2].imshow(intensity, cmap='gray', origin='upper', alpha=0.7)
    
    # Add polarization vectors (downsampled for clarity)
    step = image_size // 50  # adaptive step size
    height, width = intensity.shape
    y_indices, x_indices = np.mgrid[0:height:step, 0:width:step]
    
    for y, x in zip(y_indices.flatten(), x_indices.flatten()):
        if (y < height and x < width and 
            not np.isnan(aolp[y, x]) and dop[y, x] > 0.1):
            angle = aolp[y, x]
            length = dop[y, x] * step * 0.6
            dx = length * np.cos(angle)
            dy = length * np.sin(angle)
            axes[1, 2].arrow(x, y, dx, dy, head_width=step*0.1, head_length=step*0.1, 
                           fc='red', ec='red', alpha=0.7)
    
    axes[1, 2].set_title('Polarization Vectors')
    axes[1, 2].set_aspect('equal')
    
    plt.tight_layout()
    plt.show()

# Example usage
if __name__ == "__main__":
    # Create sky image with fisheye projection
    intensity, dop, aolp = create_fisheye_sky_image(
        image_size=1683,
        sun_altitude=90,    # 45 degrees above horizon
        sun_azimuth=135,    # Southeast
        turbidity=2.0
    )
    
    # Visualize results
    visualize_results(intensity, dop, aolp, sun_altitude=45, sun_azimuth=135)
    
    # You can also save the images
    np.save('sky_intensity.npy', intensity)
    np.save('sky_dop.npy', dop)
    np.save('sky_aolp.npy', aolp)
    
    print("Sky images generated successfully!")
    print(f"Image shape: {intensity.shape}")
    print(f"Intensity range: {intensity.min():.3f} to {intensity.max():.3f}")
    print(f"DoLP range: {dop.min():.3f} to {dop.max():.3f}")
    print(f"AoLP range: {np.nanmin(aolp):.3f} to {np.nanmax(aolp):.3f}")
