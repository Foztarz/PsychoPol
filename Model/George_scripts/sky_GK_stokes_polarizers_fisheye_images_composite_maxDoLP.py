#!/usr/bin/env python

__author__ = "Evripidis Gkanias"
__copyright__ = "Copyright (c) 2019, Insect Robotics Group," \
                "Institude of Perception, Action and Behaviour," \
                "School of Informatics, the University of Edinburgh"
__credits__ = ["Evripidis Gkanias"]
__license__ = "MIT"
__version__ = "1.0.1"
__maintainer__ = "Evripidis Gkanias"


from base import Environment
from sphere.transform import tilt
from utils import eps
import os
import matplotlib.pyplot as plt
import sys
import numpy as np

# Transformation matrix of turbidity to luminance coefficients
T_L = np.array([[ 0.1787, -1.4630],
                [-0.3554,  0.4275],
                [-0.0227,  5.3251],
                [ 0.1206, -2.5771],
                [-0.0670,  0.3703]])


class Sky(Environment):
    """
    The Sky environment class. This environment class provides skylight cues.
    """

    def __init__(self, theta_s=0., phi_s=0., theta_t=0., phi_t=0., name="sky"):
        """

        :param theta_s: sun elevation (distance from zenith)
        :param phi_s: sun azimuth (clockwise from North)
        :param theta_t: elevation of observer's zenith point with respect to the sky zenith point
        :param phi_t: azimuth of observer's zenith point with respect to the sky zenith point
        """
        super(Sky, self).__init__(name=name)
        self.__a, self.__b, self.__c, self.__d, self.__e = 0., 0., 0., 0., 0.
        self.__tau_L = 2.  # type: float
        self._update_luminance_coefficients(self.__tau_L)
        self.__c1 = .6  # type: float
        self.__c2 = 4.  # type: float
        self.theta_s = theta_s
        self.phi_s = phi_s
        self.theta_t = theta_t
        self.phi_t = phi_t

        self.__theta = np.full(1, np.nan)  # type: np.ndarray
        self.__phi = np.full(1, np.nan)  # type: np.ndarray
        self.__aop = np.full(1, np.nan)  # type: np.ndarray
        self.__dop = np.full(1, np.nan)  # type: np.ndarray
        self.__y = np.full(1, np.nan)  # type: np.ndarray
        self.__eta = np.full(1, False)  # type: np.ndarray

        self.verbose = False  # type: bool
        self.__is_generated = False  # type: bool

    def __call__(self, theta=None, phi=None, noise=0., eta=None, uniform_polariser=False):
        """
        Call the sky instance to generate the sky cues.

        :param theta: array of points' elevation
        :type theta: np.ndarray
        :param phi: array of points' azimuth
        :type phi: np.ndarray
        :param noise: the noise level (sigma)
        :type noise: float
        :param eta: array of noise level in each point of interest
        :type eta: np.ndarray
        :param uniform_polariser:
        :type uniform_polariser: bool
        :return: Y, P, A
        """

        # set default arguments
        theta = ((self.__theta if theta is None else theta) + np.pi) % (2 * np.pi) - np.pi
        phi = ((self.__phi if phi is None else phi) + np.pi) % (2 * np.pi) - np.pi

        # save points of interest
        self.__theta = theta.copy()
        self.__phi = phi.copy()

        # transform points in the sky according to tilting parameters
        theta, phi = tilt(self.theta_t, self.phi_t + np.pi, theta=theta, phi=phi)
        theta_s, phi_s = self.theta_s, self.phi_s

        # SKY INTEGRATION
        gamma = np.arccos(np.cos(theta) * np.cos(theta_s) +
                          np.sin(theta) * np.sin(theta_s) * np.cos(phi - phi_s))

        # Intensity
        i_prez = self.L(gamma, theta)
        i_00 = self.L(0., theta_s)  # the luminance (Cd/m^2) at the zenith point
        i_90 = self.L(np.pi / 2, np.absolute(theta_s - np.pi / 2))  # the luminance (Cd/m^2) on the horizon
        # influence of sky intensity
        i = (1. / (i_prez + eps) - 1. / (i_00 + eps)) * i_00 * i_90 / (i_00 - i_90 + eps)
        if uniform_polariser:
            y = np.maximum(np.full_like(i_prez, self.Y_z), 0.)
        else:
            y = np.maximum(self.Y_z * i_prez / (i_00 + eps), 0.)  # Illumination

        # Degree of Polarisation
        lp = np.square(np.sin(gamma)) / (1 + np.square(np.cos(gamma)))
        if uniform_polariser:
            p = np.ones_like(lp)
        else:
            p = np.clip(2. / np.pi * self.M_p * lp * (theta * np.cos(theta) + (np.pi / 2 - theta) * i_prez), 0., 1.)

        # Angle of polarisation
        if uniform_polariser:
            a = np.full_like(p, phi_s + np.pi)
        else:
            _, a = tilt(theta_s, phi_s + np.pi, theta, phi)

        # create cloud disturbance
        if eta is None:
            if type(noise) is np.ndarray:
                if noise.size == p.size:
                    # print "yeah!"
                    eta = np.array(noise, dtype=bool)
                    if self.verbose:
                        print("Noise level: %.4f (%.2f %%)" % (noise, 100. * eta.sum() / float(eta.size)))
                else:
                    eta = np.zeros_like(theta, dtype=bool)
                    eta[:noise.size] = noise
            elif noise > 0:
                eta = np.argsort(np.absolute(np.random.randn(*p.shape)))[:int(noise * p.shape[0])]
            else:
                eta = np.zeros_like(theta, dtype=bool)
        y[eta] = 0.
        p[eta] = 0.  # destroy the polarisation pattern
        a[eta] = np.nan

        self.__y = y
        self.__dop = p
        self.__aop = a
        self.__eta = eta

        self.__is_generated = True

        return y, p, a

    def L(self, chi, z):
        """
        Prez. et. al. Luminance function.
        Combines the scattering indicatrix and luminance gradation functions to compute the total
        luminance observed at the given sky element(s).

        :param chi: angular distance between the observed element and the sun location -- [0, pi]
        :param z: angular distance between the observed element and the zenith point -- [0, pi/2]
        :return: the total observed luminance (Cd/m^2) at the given element(s)
        """
        z = np.array(z)
        i = z < (np.pi / 2)
        f = np.zeros_like(z)
        if z.ndim > 0:
            f[i] = (1. + self.A * np.exp(self.B / (np.cos(z[i]) + eps)))
        elif i:
            f = (1. + self.A * np.exp(self.B / (np.cos(z) + eps)))
        phi = (1. + self.C * np.exp(self.D * chi) + self.E * np.square(np.cos(chi)))
        return f * phi

    @property
    def A(self):
        """
        A: Darkening or brightening of the horizon
        """
        return self.__a

    @A.setter
    def A(self, value):
        """
        :param value: Darkening or brightening of the horizon
        """
        self.__a = value
        self._update_turbidity(self.A, self.B, self.C, self.D, self.E)
        self.__is_generated = False

    @property
    def B(self):
        """
        B: Luminance gradient near the horizon
        """
        return self.__b

    @B.setter
    def B(self, value):
        """
        :param value: Luminance gradient near the horizon
        """
        self.__b = value
        self._update_turbidity(self.A, self.B, self.C, self.D, self.E)
        self.__is_generated = False

    @property
    def C(self):
        """
        C: Relative intensity of the circumsolar region
        """
        return self.__c

    @C.setter
    def C(self, value):
        """
        :param value: Relative intensity of the circumsolar region
        """
        self.__c = value
        self._update_turbidity(self.A, self.B, self.C, self.D, self.E)
        self.__is_generated = False

    @property
    def D(self):
        """
        D: Width of the circumsolar region
        """
        return self.__d

    @D.setter
    def D(self, value):
        """
        :param value: Width of the circumsolar region
        """
        self.__d = value
        self._update_turbidity(self.A, self.B, self.C, self.D, self.E)
        self.__is_generated = False

    @property
    def E(self):
        """
        E: relative backscattered light
        """
        return self.__e

    @E.setter
    def E(self, value):
        """
        :param value: relative backscattered light
        """
        self.__e = value
        self._update_turbidity(self.A, self.B, self.C, self.D, self.E)
        self.__is_generated = False

    @property
    def c1(self):
        """
        :return: 1st coefficient of the maximum degree of polarisation
        """
        return self.__c1

    @property
    def c2(self):
        """
        :return: 2nd coefficient of the maximum degree of polarisation
        """
        return self.__c2

    @property
    def tau_L(self):
        """
        :return: turbidity
        """
        return self.__tau_L

    @tau_L.setter
    def tau_L(self, value):
        """
        :param value: turbidity
        """
        assert value >= 1., "Turbidity must be greater or eaqual to 1."
        self.__is_generated = self.__tau_L == value and self.__is_generated
        self._update_luminance_coefficients(value)

    @property
    def Y_z(self):
        """
        :return: the zenith luminance (K cd/m^2)
        """
        chi = (4. / 9. - self.tau_L / 120.) * (np.pi - 2 * self.theta_s)
        return (4.0453 * self.tau_L - 4.9710) * np.tan(chi) - 0.2155 * self.tau_L + 2.4192

    @property
    def M_p(self):
        """
        :return: maximum degree of polarisation
        """
        return np.exp(-(self.tau_L - self.c1) / (self.c2 + eps))

    @property
    def Y(self):
        """
        :return: luminance of the sky (K cd/m^2)
        """
        assert self.__is_generated, "Sky is not generated yet. In order to generate the sky, use the call function."
        return self.__y

    @property
    def DOP(self):
        """
        :return: the linear degree of polarisation in the sky
        """
        assert self.__is_generated, "Sky is not generated yet. In order to generate the sky, use the call function."
        return self.__dop

    @property
    def AOP(self):
        """
        :return: the angle of linear polarisation in the sky
        """
        assert self.__is_generated, "Sky is not generated yet. In order to generate the sky, use the call function."
        return self.__aop

    @property
    def theta(self):
        assert self.__is_generated, "Sky is not generated yet. In order to generate the sky, use the call function."
        return self.__theta

    @theta.setter
    def theta(self, value):
        self.__theta = value
        self.__is_generated = False

    @property
    def phi(self):
        assert self.__is_generated, "Sky is not generated yet. In order to generate the sky, use the call function."
        return self.__phi

    @phi.setter
    def phi(self, value):
        self.__phi = value
        self.__is_generated = False

    @property
    def eta(self):
        assert self.__is_generated, "Sky is not generated yet. In order to generate the sky, use the call function."
        return self.__eta

    @eta.setter
    def eta(self, value):
        self.__eta = value
        self.__is_generated = False

    def _update_luminance_coefficients(self, tau_L):
        self.__a, self.__b, self.__c, self.__d, self.__e = T_L.dot(np.array([tau_L, 1.]))
        self._update_turbidity(self.A, self.B, self.C, self.D, self.E)

    def _update_turbidity(self, a, b, c, d, e):
        T_T = np.linalg.pinv(T_L)
        tau_L, c = T_T.dot(np.array([a, b, c, d, e]))
        self.__tau_L = tau_L / c  # turbidity correction

    def copy(self):
        sky = Sky()
        sky.tau_L = self.tau_L
        sky.theta_s = self.theta_s
        sky.phi_s = self.phi_s
        sky.__c1 = self.__c1
        sky.__c2 = self.__c2
        sky.verbose = self.verbose

        sky.__theta = self.__theta
        sky.__phi = self.__phi
        sky.__aop = self.__aop
        sky.__dop = self.__dop
        sky.__y = self.__y
        sky.__eta = self.__eta

        sky.__is_generated = False
        return sky

    @staticmethod
    def from_observer(obs=None, date=None, yaw=0., theta_t=0., phi_t=0.):
        """
        Creates sky using an Ephem observer (Requires Ephem library)
        :param obs: the observer (location on Earth)
        :param date: the date of the observation
        :param yaw: the heading orientation of the observer
        :param theta_t: the heading tilt (pitch)
        :param phi_t: the heading tilt (roll)
        :return:
        """
        from ephem import Sun
        from datetime import datetime
        from utils import get_seville_observer

        sun = Sun()
        if obs is None:
            obs = get_seville_observer()
            obs.date = datetime(2017, 6, 21, 10, 0, 0) if date is None else date
        sun.compute(obs)
        theta_s, phi_s = np.pi/2 - sun.alt, (sun.az - yaw + np.pi) % (2 * np.pi) - np.pi

        return Sky(theta_s=theta_s, phi_s=phi_s, theta_t=theta_t, phi_t=phi_t)

    @staticmethod
    def from_type(sky_type):
        """

        :param sky_type:
        :return:
        """
        import os
        import yaml

        dir = os.path.dirname(os.path.realpath(__file__))
        with open(dir + "/standard-parameters.yaml", 'r') as f:
            try:
                sp = yaml.load(f)
            except yaml.YAMLError as exc:
                print("Could not load the sky types.", exc)
                return None

        rep = sp['type'][sky_type-1]
        a = sp['gradation'][rep['gradation']]['a']
        b = sp['gradation'][rep['gradation']]['b']
        c = sp['indicatrix'][rep['indicatrix']]['c']
        d = sp['indicatrix'][rep['indicatrix']]['d']
        e = sp['indicatrix'][rep['indicatrix']]['e']

        s = Sky()
        s._update_turbidity(a, b, c, d, e)
        # s.__tau_L = 2.

        for description in rep['description']:
            print(description)

        return s

def compute_stokes(sky):
    """
    Compute Stokes parameters from the sky object.
    S0 = Y
    S1 = S0 * DoLP * cos(2*AoLP)
    S2 = S0 * DoLP * sin(2*AoLP)
    """
    S0 = sky.Y
    S1 = S0 * sky.DOP * np.cos(2 * sky.AOP)
    S2 = S0 * sky.DOP * np.sin(2 * sky.AOP)
    return S0, S1, S2

def polarizer_intensity(S0, S1, S2, alpha):
    """
    Computes transmitted intensity through a linear polarizer
    at angle alpha using:
    I(alpha) = 0.5 * (S0 + S1*cos(2a) + S2*sin(2a))
    """
    return 0.5 * (S0 + S1 * np.cos(2*alpha) + S2 * np.sin(2*alpha))

def visualise_polarizer(sky, alpha, title=None):
    import matplotlib.pyplot as plt

    # compute stokes
    S0, S1, S2 = compute_stokes(sky)
    I = polarizer_intensity(S0, S1, S2, alpha)

    if title is None:
        title = f"Polarizer {np.rad2deg(alpha):.0f}°"

    plt.figure(title, figsize=(4.5, 4.5))
    ax = plt.subplot(111, polar=True)
    ax.set_theta_zero_location("N")
    ax.set_theta_direction(-1)

    theta_s, phi_s = tilt(sky.theta_t, sky.phi_t,
                          theta=sky.theta_s, phi=sky.phi_s)

    ax.scatter(sky.phi, sky.theta, s=10, c=I, marker='.', cmap='inferno')
    ax.scatter(phi_s, theta_s, s=100, edgecolor='black', facecolor='yellow')

    ax.set_ylim([0, np.pi/2])
    ax.set_yticks([])
    ax.set_xticks(np.linspace(0, 2*np.pi, 8, endpoint=False))
    ax.set_xticklabels([r'$0^\circ$ (N)', r'$45^\circ$ (NE)', r'$90^\circ$ (E)',
                        r'$135^\circ$ (SE)', r'$180^\circ$ (S)',
                        r'$-135^\circ$ (SW)', r'$-90^\circ$ (W)',
                        r'$-45^\circ$ (NW)'])

    plt.show()

def visualise_all_polarizers(sky):
    angles = [0, 45, 90, 135]
    for deg in angles:
        visualise_polarizer(sky, np.deg2rad(deg),
                            title=f"Polarizer {deg}°")

def visualise_stokes_component(sky, S, name, cmap='viridis'):
    import matplotlib.pyplot as plt

    plt.figure(name, figsize=(4.5, 4.5))
    ax = plt.subplot(111, polar=True)
    ax.set_theta_zero_location("N")
    ax.set_theta_direction(-1)

    theta_s, phi_s = tilt(sky.theta_t, sky.phi_t, theta=sky.theta_s, phi=sky.phi_s)

    ax.scatter(sky.phi, sky.theta, s=10, c=S, marker='.', cmap=cmap)
    ax.scatter(phi_s, theta_s, s=100, edgecolor='black', facecolor='yellow')

    ax.set_ylim([0, np.pi/2])
    ax.set_yticks([])
    ax.set_xticks(np.linspace(0, 2*np.pi, 8, endpoint=False))
    ax.set_xticklabels([r'$0^\circ$ (N)', r'$45^\circ$ (NE)', r'$90^\circ$ (E)',
                        r'$135^\circ$ (SE)', r'$180^\circ$ (S)', r'$-135^\circ$ (SW)',
                        r'$-90^\circ$ (W)', r'$-45^\circ$ (NW)'])

    plt.show()

def visualise_stokes(sky):
    S0, S1, S2 = compute_stokes(sky)
    visualise_stokes_component(sky, S0, "Stokes S0 (Intensity)", cmap='Blues_r')
    visualise_stokes_component(sky, S1, "Stokes S1")
    visualise_stokes_component(sky, S2, "Stokes S2")

def visualise_luminance(sky):
    import matplotlib.pyplot as plt

    plt.figure("Luminance", figsize=(4.5, 4.5))
    ax = plt.subplot(111, polar=True)
    ax.set_theta_zero_location("N")
    ax.set_theta_direction(-1)

    theta_s, phi_s = tilt(sky.theta_t, sky.phi_t, theta=sky.theta_s, phi=sky.phi_s)
    ax.scatter(sky.phi, sky.theta, s=20, c=sky.Y, marker='.', cmap='Blues_r', vmin=0, vmax=6)
    ax.scatter(phi_s, theta_s, s=100, edgecolor='black', facecolor='yellow')
    # ax.scatter(sky.phi_t + np.pi, sky.theta_t, s=200, edgecolor='black', facecolor='greenyellow')
    ax.set_ylim([0, np.pi/2])
    ax.set_yticks([])
    ax.set_xticks(np.linspace(0, 2*np.pi, 8, endpoint=False))
    ax.set_xticklabels([r'$0^\circ$ (N)', r'$45^\circ$ (NE)', r'$90^\circ$ (E)', r'$135^\circ$ (SE)',
                        r'$180^\circ$ (S)', r'$-135^\circ$ (SW)', r'$-90^\circ$ (W)', r'$-45^\circ$ (NW)'])

    plt.show()


def visualise_degree_of_polarisation(sky):
    import matplotlib.pyplot as plt

    plt.figure("degree-of-polarisation", figsize=(4.5, 4.5))
    ax = plt.subplot(111, polar=True)
    ax.set_theta_zero_location("N")
    ax.set_theta_direction(-1)

    theta_s, phi_s = tilt(sky.theta_t, sky.phi_t, theta=sky.theta_s, phi=sky.phi_s)
    print(theta_s, phi_s)
    ax.scatter(sky.phi, sky.theta, s=10, c=sky.DOP, marker='.', cmap='Greys', vmin=0, vmax=1)
    ax.scatter(phi_s, theta_s, s=100, edgecolor='black', facecolor='yellow')
    # ax.scatter(sky.phi_t + np.pi, sky.theta_t, s=200, edgecolor='black', facecolor='greenyellow')
    ax.set_ylim([0, np.pi/2])
    ax.set_yticks([])
    ax.set_xticks(np.linspace(0, 2*np.pi, 8, endpoint=False))
    ax.set_xticklabels([r'$0^\circ$ (N)', r'$45^\circ$ (NE)', r'$90^\circ$ (E)', r'$135^\circ$ (SE)',
                        r'$180^\circ$ (S)', r'$-135^\circ$ (SW)', r'$-90^\circ$ (W)', r'$-45^\circ$ (NW)'])

    plt.show()


def visualise_angle_of_polarisation(sky):
    import matplotlib.pyplot as plt

    plt.figure("angle-of-polarisation", figsize=(4.5, 4.5))
    ax = plt.subplot(111, polar=True)
    ax.set_theta_zero_location("N")
    ax.set_theta_direction(-1)

    theta_s, phi_s = tilt(sky.theta_t, sky.phi_t, theta=sky.theta_s, phi=sky.phi_s)
    print(theta_s, phi_s)
    ax.scatter(sky.phi, sky.theta, s=10, c=sky.AOP, marker='.', cmap='hsv', vmin=-np.pi, vmax=np.pi)
    ax.scatter(phi_s, theta_s, s=100, edgecolor='black', facecolor='yellow')
    # ax.scatter(sky.phi_t + np.pi, sky.theta_t, s=200, edgecolor='black', facecolor='greenyellow')
    ax.set_ylim([0, np.pi/2])
    ax.set_yticks([])
    ax.set_xticks(np.linspace(0, 2*np.pi, 8, endpoint=False))
    ax.set_xticklabels([r'$0^\circ$ (N)', r'$45^\circ$ (NE)', r'$90^\circ$ (E)', r'$135^\circ$ (SE)',
                        r'$180^\circ$ (S)', r'$-135^\circ$ (SW)', r'$-90^\circ$ (W)', r'$-45^\circ$ (NW)'])

    plt.show()

def generate_fisheye_grid(resolution=512):
    """
    Generates azimuthal equidistant grid (radians) for fisheye projection.
    Returns theta, phi arrays of shape (res, res)
    """
    import numpy as np

    # create 2D grid from -1..1 in x and y
    x = np.linspace(-1, 1, resolution)
    y = np.linspace(-1, 1, resolution)
    xv, yv = np.meshgrid(x, y)

    # radius from center
    r = np.sqrt(xv**2 + yv**2)
    r_clip = np.clip(r, 0, 1)  # limit to unit circle

    # azimuthal angle
    phi = np.arctan2(yv, xv)  # -pi .. pi

    # zenith distance theta
    theta = r_clip * (np.pi/2)  # 0 at center, pi/2 at edge

    # mask outside unit circle
    mask = r <= 1

    return theta, phi, mask

import numpy as np


def fisheye_sky_image(sky, resolution=512, channel='S0', polarizer_angle=None):
    """
    Vectorized per-pixel fisheye image generation.
    """
    # center and radius
    cx, cy = resolution/2, resolution/2
    radius_max = resolution/2

    # create pixel coordinates
    j, i = np.meshgrid(np.arange(resolution), np.arange(resolution))
    x = (j - cx)/radius_max
    y = (i - cy)/radius_max
    r = np.sqrt(x**2 + y**2)
    mask = r <= 1  # only inside unit circle

    # azimuth and elevation (theta/phi)
    phi = np.arctan2(x, -y)
    theta = r * (np.pi/2)

    # flatten arrays for sky evaluation
    theta_flat = theta[mask].ravel()
    phi_flat = phi[mask].ravel()

    # evaluate sky for all pixels at once
    Y, DOP, AoLP = sky(theta_flat, phi_flat)
    AoLP = -AoLP ##########################################
    # create image array
    img = np.zeros((resolution, resolution))

    if channel == 'S0':
        img[mask] = Y
    elif channel == 'DOP':
        img[mask] = DOP
    elif channel == 'AoLP':
        img[mask] = AoLP
    elif channel == 'polarizer':
        alpha = np.deg2rad(polarizer_angle)
        img[mask] = Y * (1 + DOP * np.cos(2*(AoLP - alpha)))

    return img



def ensure_folder(folder):
    if not os.path.exists(folder):
        os.makedirs(folder)

def save_fisheye_image(img, folder, name, cmap='viridis', vmin=None, vmax=None):
    """
    Save a fisheye image as both .npy and .png with exact array resolution.
    """
    import os
    import numpy as np
    import matplotlib.pyplot as plt

    # ensure folder exists
    if not os.path.exists(folder):
        os.makedirs(folder)

    # save numpy array
    np.save(os.path.join(folder, name + '.npy'), img)

    # save PNG using imsave for exact resolution
    plt.imsave(os.path.join(folder, name + '.png'), img, cmap=cmap, vmin=vmin, vmax=vmax)



def save_fisheye_image_with_colorbar(img, folder, name, cmap='viridis', vmin=None, vmax=None):
    """
    Save a fisheye image as both .npy and .png with a colorbar.
    """
    if not os.path.exists(folder):
        os.makedirs(folder)

    # save numpy array
    np.save(os.path.join(folder, name + '.npy'), img)

    # save PNG with colorbar
    plt.figure(figsize=(6,6))
    plt.imshow(img, origin='lower', cmap=cmap, vmin=vmin, vmax=vmax)
    plt.axis('off')
    plt.colorbar(fraction=0.046, pad=0.04)
    plt.tight_layout()
    plt.savefig(os.path.join(folder, name + '_colorbar.png'), dpi=150)
    plt.close()

def save_fisheye_sky(sky, resolution=512, folder='sky_outputs'):
    S0, S1, S2 = compute_stokes(sky)
    channels = {
        'S0': S0,
        'DOP': sky.DOP,
        'AoLP': sky.AOP,
        'S1': S1,
        'S2': S2
    }

    for name, vals in channels.items():
        img = fisheye_sky_image(sky, resolution=resolution, channel='S0')  # or 'DOP', 'AoLP', etc.
        cmap = 'hsv' if name == 'AoLP' else 'viridis'
        save_fisheye_image(img, folder, name, cmap=cmap)

    # polarizers
    angles = [0, 45, 90, 135]
    for a in angles:
        I = polarizer_intensity(S0, S1, S2, np.deg2rad(a))
        img = fisheye_sky_image(sky, resolution=resolution, channel='S0')  # or 'DOP', 'AoLP', etc.
        save_fisheye_image(img, folder, f'polarizer_{a}', cmap='inferno')


if __name__ == "__main__":
    import numpy as np
    import os
    import sys
    import matplotlib.pyplot as plt
    from matplotlib import cm

    resolution = 512
    folder = "fisheye_output_composite"
    os.makedirs(folder, exist_ok=True)

    azimuth = 0.0
    elevations = np.arange(0, 91, 1)

    # -------------------------------------------------------
    # 0. Precompute fisheye mask
    # -------------------------------------------------------
    theta_grid, phi_grid, fisheye_mask = generate_fisheye_grid(resolution)

    # -------------------------------------------------------
    # 1. Initialize composite storage
    # -------------------------------------------------------
    shape = (resolution, resolution)

    DOP_max   = np.full(shape, -np.inf)
    S0_best   = np.zeros(shape)
    AoLP_best = np.zeros(shape)

    img0_best   = np.zeros(shape)
    img45_best  = np.zeros(shape)
    img90_best  = np.zeros(shape)
    img135_best = np.zeros(shape)

    elev_best = np.zeros(shape)

    # -------------------------------------------------------
    # 1B. Output folder for per-elevation frames
    # -------------------------------------------------------
    frames_folder = os.path.join(folder, "frames")
    os.makedirs(frames_folder, exist_ok=True)

    cmap = cm.get_cmap("turbo")
    norm_min = elevations.min()
    norm_max = elevations.max()

    # -------------------------------------------------------
    # 2. Elevation sweep
    # -------------------------------------------------------
    for elev in elevations:
        print(f"Processing elevation: {elev}°")

        s = Sky(
            theta_s=np.radians(90 - elev),
            phi_s=-np.radians(azimuth)
        )

        # Generate fisheye channels
        S0_img   = fisheye_sky_image(s, resolution, channel='S0')
        DOP_img  = fisheye_sky_image(s, resolution, channel='DOP')
        AoLP_img = fisheye_sky_image(s, resolution, channel='AoLP')

        img0   = fisheye_sky_image(s, resolution, channel='polarizer', polarizer_angle=0)
        img45  = fisheye_sky_image(s, resolution, channel='polarizer', polarizer_angle=45)
        img90  = fisheye_sky_image(s, resolution, channel='polarizer', polarizer_angle=90)
        img135 = fisheye_sky_image(s, resolution, channel='polarizer', polarizer_angle=135)

        # Pixel-wise winner update
        winner_mask = (DOP_img > DOP_max)

        DOP_max[winner_mask]   = DOP_img[winner_mask]
        S0_best[winner_mask]   = S0_img[winner_mask]
        AoLP_best[winner_mask] = AoLP_img[winner_mask]

        img0_best[winner_mask]   = img0[winner_mask]
        img45_best[winner_mask]  = img45[winner_mask]
        img90_best[winner_mask]  = img90[winner_mask]
        img135_best[winner_mask] = img135[winner_mask]

        elev_best[winner_mask] = elev

        # -------------------------------------------------------
        # 3B. Save a standalone frame image (PNG)
        # -------------------------------------------------------
        e_norm = (elev_best - norm_min) / (norm_max - norm_min)
        e_norm = np.clip(e_norm, 0, 1)

        rgb = cmap(e_norm)[..., :3]
        rgb[~fisheye_mask] = 0

        frame = (rgb * 255).astype(np.uint8)

        frame_path = os.path.join(frames_folder, f"frame_{elev:03d}.png")
        plt.imsave(frame_path, frame)

    # -------------------------------------------------------
    # 4. Save composite maps
    # -------------------------------------------------------
    save_fisheye_image(S0_best,   folder, "S0_COMPOSITE",   cmap='gray')
    save_fisheye_image(DOP_max,   folder, "DOP_COMPOSITE",  cmap='viridis')
    save_fisheye_image(AoLP_best, folder, "AoLP_COMPOSITE", cmap='hsv')

    save_fisheye_image(img0_best,   folder, "img_000_COMPOSITE", cmap='gray')
    save_fisheye_image(img45_best,  folder, "img_045_COMPOSITE", cmap='gray')
    save_fisheye_image(img90_best,  folder, "img_090_COMPOSITE", cmap='gray')
    save_fisheye_image(img135_best, folder, "img_135_COMPOSITE", cmap='gray')

    save_fisheye_image(elev_best,  folder, "ELEVATION_WINNER", cmap='turbo')

    save_fisheye_image_with_colorbar(DOP_max,   folder, "DOP_COMPOSITE",  cmap='viridis')
    save_fisheye_image_with_colorbar(AoLP_best, folder, "AoLP_COMPOSITE", cmap='hsv')
    save_fisheye_image_with_colorbar(elev_best, folder, "ELEVATION_WINNER", cmap='turbo')

    print("🖼️ Saved per-elevation frames!")
    print("✅ Composite sky generation complete.")




