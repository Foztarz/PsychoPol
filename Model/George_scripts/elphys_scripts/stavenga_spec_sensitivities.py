import numpy as np
import matplotlib.pyplot as plt
from scipy.interpolate import UnivariateSpline

def StavengaSpline(spec_range=(300, 700), lambda_max=340, a_type='a1'):
    """
    Generates a spline template for a visual pigment based on the Stavenga model.
    
    Parameters:
    spec_range (tuple): Bounds of the spectrum in nanometers (default is 300 to 700 nm).
    lambda_max (float): Peak sensitivity wavelength.
    a_type (str): Pigment type, currently only 'a1' is available.
    
    Returns:
    UnivariateSpline: A smoothed spline object representing the spectral sensitivity curve.
    """
    
    # wavelength range within the specified spectrum bounds
    wlns = np.linspace(spec_range[0], spec_range[1], 1000)

    # modified lognormal function
    def m_lognorm(wl, l_max, a0, a1):
        x = np.log10(wl / l_max)
        return np.exp(-a0 * x**2 * (1 + a1 * x + 3 * a1**2 * x**2))

    # spectral bands based on pigment type
    if a_type == 'a1':
        # Alpha band (primary peak)
        a_band = m_lognorm(wlns, lambda_max, 380, 6.09)
        
        # Beta band (secondary peak)
        b_band = 0.29 * m_lognorm(wlns, 340, 247, 3.59)
        
        # Gamma band (broad peak)
        g_band = 1.99 * m_lognorm(wlns, 276, 647, 23.4)
    else:
        raise ValueError("Only 'a1' pigment type is currently implemented.")
    
    # combine the bands and normalize
    r_stav = (a_band + b_band + g_band) / np.max(a_band + b_band + g_band)

    # smoothed spline over the normalized spectral sensitivity curve
    spline = UnivariateSpline(wlns, r_stav, s=0)

    return wlns, spline(wlns)  

lambda_max = 340  # peak wavelength for this pigment

wavelengths, sensitivity = StavengaSpline(lambda_max=lambda_max)

plt.figure(figsize=(10, 6))
plt.plot(wavelengths, sensitivity, label=f'Lambda max = {lambda_max} nm')
plt.xlabel("Wavelength (nm)")
plt.ylabel("Normalized Sensitivity")
plt.title("Spectral Sensitivity Curve (Stavenga Model)")
plt.legend()
plt.grid(True)
plt.show()
