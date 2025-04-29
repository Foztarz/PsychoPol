import numpy as np
import matplotlib.pyplot as plt
from scipy.interpolate import UnivariateSpline

## usage: python stavenga_spec_sensitivities.py
## change the lambda max value(s) from within the script

def StavengaSpline(spec_range=(300, 500), lambda_max=340, a_type='a1'):
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

lambda_max_1 = 340  # peak wavelength for this pigment, honeybee
lambda_max_2 = 335  # peak wavelength, bumblebee

# spectral sensitivity curves for 2 lambda_max values
wavelengths_1, sensitivity_1 = StavengaSpline(lambda_max=lambda_max_1)
wavelengths_2, sensitivity_2 = StavengaSpline(lambda_max=lambda_max_2)

# curve plotting
plt.figure(figsize=(10, 10))
plt.plot(wavelengths_1, sensitivity_1, label=f'Lambda max = {lambda_max_1} nm', color='#FF9999')
plt.plot(wavelengths_2, sensitivity_2, label=f'Lambda max = {lambda_max_2} nm', color='#ADD8E6')
plt.xlabel("Wavelength (nm)")
plt.ylabel("Normalized Sensitivity")
plt.title("Spectral Sensitivity Curves (Stavenga Model)")
plt.legend()
plt.grid(True)
plt.tick_params(axis='both', labelsize=16)
plt.savefig('spectral_sensitivity_curves.svg', format='svg')
plt.show()
