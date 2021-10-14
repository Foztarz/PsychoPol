# -*- coding: utf-8 -*-
"""
Created on Thu Oct 14 14:35:06 2021

@author: jaf54iq
"""

import invertpy
import numpy as np
import os
# import invertsy # error in invertsy.agent prevents import
"""
DIY version of invertsy.env.sky
"""

# from invertsy.__helpers import eps, __root__
RNG = np.random.RandomState(2021)
"""
The defaults random value generator.
"""
eps = np.finfo(float).eps
"""
The smallest non-zero positive.
"""
# __root__ = os.path.realpath(os.path.join(os.path.dirname(os.path.realpath(__file__)), "..", ".."))

# from ._helpers import add_noise, RNG

def add_noise(v=None, noise=0., shape=None, fill_value=0, rng=RNG):
    if shape is None and v is not None:
        shape = v.shape
    if shape is not None:
        size = np.sum(shape)
    elif v is not None:
        size = v.size
    else:
        size = None
    if isinstance(noise, np.ndarray):
        if size is None or noise.size == size:
            eta = np.array(noise, dtype=bool)
        else:
            eta = np.zeros(shape, dtype=bool)
            eta[:noise.size] = noise
    elif noise > 0:
        if shape is not None:
            eta = np.argsort(np.absolute(rng.randn(*shape)))[:int(noise * shape[0])]
        else:
            eta = rng.randn()
    else:
        eta = np.zeros_like(v, dtype=bool)

    if v is not None:
        v[eta] = fill_value

    return eta

# from .observer import Observer, get_seville_observer
from datetime import datetime, tzinfo
from pytz import timezone
from copy import copy

# import numpy as np

class Observer(object):
    def __init__(self, lon=None, lat=None, date=datetime.now(), city=None, degrees=False):
        """
        The observer on Earth holds information about its longitude and latitude, date and time.

        Parameters
        ----------
        lon: float, optional
            the longitude of the observer. Default is None
        lat: float, optional
            the latitude of the observer. Default is None
        date: datetime, optional
            the date and time of the event. Default is the current date and time
        city: str, optional
            the name of the city if available. Default is None
        degrees: bool, optional
            True if the longitude and latitude are given in degrees, False otherwise. This will also affect the form
            that they will be returned as attributes. Default is False
        """
        if lon is not None and lat is not None:
            self._lon = float(lon) if not degrees else np.deg2rad(float(lon))
            self._lat = float(lat) if not degrees else np.deg2rad(float(lat))
        else:
            self._lon = lon
            self._lat = lat
        self._date = date
        self._city = city
        h_loc = date.hour
        h_gmt = date.astimezone(timezone("GMT")).hour
        self._tzinfo = date.tzinfo
        self._tz = h_loc - h_gmt
        self.on_change = None
        self.__inrad = not degrees

    @property
    def lon(self):
        """
        The longitude of the observer.

        Returns
        -------
        float
        """
        return self._lon if self.__inrad else np.rad2deg(self._lon)

    @lon.setter
    def lon(self, value):
        """

        Parameters
        ----------
        value: float, int, str
        """
        self._lon = float(value) if self.__inrad else np.deg2rad(float(value))
        if self.on_change is not None:
            self.on_change()

    @property
    def lat(self):
        """
        The latitude of the observer.

        Returns
        -------
        float
        """
        return self._lat if self.__inrad else np.rad2deg(self._lat)

    @lat.setter
    def lat(self, value):
        """
        Parameters
        ----------
        value: float, int, str
        """
        self._lat = float(value) if self.__inrad else np.deg2rad(float(value))
        if self.on_change is not None:
            self.on_change()

    @property
    def tzgmt(self):
        """
        The difference in hours from the GMT timezone.

        Returns
        -------
        int
        """
        return self._tz

    @property
    def timezone(self):
        """
        Information about the timezone.

        Returns
        -------
        tzinfo
        """
        return self._tzinfo

    @property
    def date(self):
        """
        The date and time in the current position.

        Returns
        -------
        datetime
        """
        return self._date

    @date.setter
    def date(self, value):
        """
        Parameters
        ----------
        value: datetime
        """
        self._date = value
        if self.on_change is not None:
            self.on_change()

    @property
    def city(self) -> str:
        """
        The closest city to the current location

        Returns
        -------
        str
        """
        return self._city

    def copy(self):
        """
        Creates a copy of the observer.

        Returns
        -------
        Observer
        """
        return copy(self)

    def __copy__(self):
        return Observer(lon=copy(self.lon), lat=copy(self.lat), degrees=not copy(self.__inrad),
                        date=copy(self.date), city=copy(self.city))

    def __repr__(self):
        return "Observer(lon='%.6f', lat='%.6f', %sdate='%s', timezone='%s')" % (
            self.lon, self.lat, ("city='%s', " % self.city) if self._city is not None else "",
            str(self._date), self.timezone)


def get_seville_observer():
    """
    Creates an observer with the properties of Seville in Spain and with the current date and time.

    - latitude: 37.392509
    - longitude: -5.983877

    Returns
    -------
    Observer
    """
    sev = Observer()
    sev.lat = '37.392509'
    sev.lon = '-5.983877'
    sev._city = "Seville"

    return sev
# from .ephemeris import Sun

class Sun(object):
    def __init__(self, observer=None):
        """
        Instance of the sun based on the observer on Earth. The observer contains information like their longitude and
        latitude and the date and time, which are used by the sun in order to compute it position with respect to the
        observer.

        Parameters
        ----------
        observer: Observer
            the observer who observes the sun
        """
        self._jd = 0.
        """The Julian day"""
        self._srv = 0.
        """The solar relative vector"""

        self._sd = 0.
        """
        The declination angle.
        """
        self._eot = 0.
        """
        The Equation of Time (EoT) (in minutes) is an empirical equation that corrects for the eccentricity of the
        Earth's orbit and the Earth's axial tilt
        """
        self._sn = 0.
        """The solar noon."""
        self._srt = 0.
        """The sunrise relative time."""
        self._sst = 0.
        """The sunset relative time."""
        self._sld = 0.
        """The duration of teh sunlight."""
        self._sea = 0.
        """Solar elevation without the correction for atmospheric refraction."""
        self._aar = 0.
        """The approximate atmospheric refraction."""
        self._hra = 0.
        """The hour angle."""
        self._tst = 0.
        """The true solar time."""

        self._alt = 0.
        """The altitude of the sun (rads). Solar elevation (altitude) corrected for atmospheric refraction."""
        self._azi = 0.
        """The azimuth of the sun (rads)."""
        self._is_ready = False
        """If all the parameters has been computed based on the updated observer."""

        self._obs = None
        """The observer of the sun on Earth."""

        # set the observer of the sun on Earth
        if observer is not None:
            self.compute(observer)

    def compute(self, observer):
        """
        Computes all the parameters of the sun given an observer.

        Parameters
        ----------
        observer: Observer
        """
        self.obs = observer
        lon, lat = observer._lon, observer._lat

        jd = self._jd = julian_day(observer.date)
        jc = julian_century(jd)

        gmls = geom_mean_long_sun(jc)
        gmas = geom_mean_anom_sun(jc)
        eeo = eccent_earth_orbit(jc)
        seoc = sun_eq_of_ctr(jc, gmas)
        stl = sun_true_long(gmls, seoc)
        sta = sun_true_anom(gmas, seoc)
        self._srv = sun_rad_vector(eeo, sta)

        sal = sun_app_long(jc, stl)
        moe = mean_obliq_ecliptic(jc)
        oc = obliq_corr(jc, moe)
        sra = sun_rt_ascen(sal, oc)
        sd = self._sd = sun_declin(sal, oc)

        vy = var_y(oc)
        eot = self._eot = eq_of_time(gmls, gmas, eeo, vy)

        hasr = ha_sunrise(lat, sd)
        sn = self._sn = solar_noon(lon, eot, tz=self.obs.tzgmt)
        self._srt = sunrise_time(hasr, sn)
        self._sst = sunset_time(hasr, sn)
        self._sld = sunlight_duration(hasr)

        tst = self._tst = true_solar_time(lon, observer.date, eot, tz=self.obs.tzgmt)
        ha = self._hra = hour_angle(tst)
        sza = solar_zenith_angle(lat, sd, ha)
        sea = self._sea = solar_elevation_angle(sza)
        aar = self._aar = approx_atmospheric_refraction(sea)
        self._alt = solar_elevation_corrected_for_atm_refraction(sea, aar)
        self._azi = solar_azimuth_angle(lat, ha, sza, sd)

        self._is_ready = True

    def update(self):
        """
        Computes the parameters of the sun using the internal observer.
        """
        assert self.obs is not None, (
            "Observer has not been set. Please set the observer before you update the sun position."
        )

        self.compute(self.obs)

    @property
    def obs(self):
        """
        The observer who observes the sun.
        """
        return self._obs

    @obs.setter
    def obs(self, value):
        value.on_change = self.update
        self._obs = value
        self._is_ready = False

    @property
    def alt(self):
        """
        The altitude of the sun (rads). Solar elevation (altitude) corrected for atmospheric refraction.
        """
        return self._alt

    @property
    def az(self):
        """
        The azimuth of the sun (rads). Clockwise from North.
        """
        return self._azi

    @property
    def zenith_angle(self):
        """
        The angular distance of the sun from the zenith
        """
        return np.pi/2 - self._alt

    @property
    def equation_of_time(self):
        """
        The Equation of Time (EoT) (in minutes) is an empirical equation that corrects for the eccentricity of the
        Earth's orbit and the Earth's axial tilt
        """
        return self._eot

    @property
    def solar_elevation_angle(self):
        """
        Solar elevation without the correction for atmospheric refraction.
        """
        return self._sea

    @property
    def approximate_atmospheric_refraction(self):
        """
        The approximate atmospheric refraction
        """
        return self._aar

    @property
    def hour_angle(self):
        """
        The Hour Angle converts the local solar time (LST) into the number of degrees which the sun moves across the
        env. By definition, the HRA is 0° at solar noon. Since the Earth rotates 15° per hour away from solar noon
        corresponds to an angular motion of the sun in the env of 15°. In the morning the hour angle is negative, in
        the afternoon the hour angle is positive.
        """
        return self._hra

    @property
    def declination(self):
        """
        The declination angle.
        """
        return self._sd

    @property
    def sunrise(self):
        """
        The sunrise (absolute) time.
        """
        return relative_to_absolute_time(self._obs, self._srt)

    @property
    def sunset(self):
        """
        The sunset (absolute) time.
        """
        return relative_to_absolute_time(self._obs, self._sst)

    @property
    def is_ready(self):
        """
        True if the sun has been updated, otherwise False.
        """
        return self._is_ready

def julian_day(date):
    """
    The Julian day is the continuous count of days since the beginning of the Julian period, and is used primarily by
    astronomers, and in software for easily calculating elapsed days between two events.

    Parameters
    ----------
    date: datetime
        the date and time to be converted into the Julian day.

    Returns
    -------
    float
        the Julian day
    """
    return date.toordinal() + 1721424.5 + (date.hour + (date.minute + date.second / 60) / 60) / 24


def julian_century(jd):
    """
    The Julian century is the Julian day divided by 36525.

    Parameters
    ----------
    jd: float
        the Julian day

    Returns
    -------
    float
        the Julian century
    """
    return (jd - 2451545) / 36525


def geom_mean_long_sun(jc):
    """
    The geometric mean longitude of the sun (correct for aberration) at the given Julian century.

    Parameters
    ----------
    jc: float
        the Julian century

    Returns
    -------
    float
    """
    return np.deg2rad((280.46646 + jc * (36000.76983 + jc * 0.0003032)) % 360)


def geom_mean_anom_sun(jc):
    """
    The geometric mean anomaly of the sun during the given Julian century.

    Parameters
    ----------
    jc: float
        the Julian century

    Returns
    -------
    float
    """
    return np.deg2rad(357.52911 + jc * (35999.05029 - 0.0001537 * jc))


def eccent_earth_orbit(jc):
    """
    Eccentricity of Earth's orbit. Inclination of the plane of the Earth's orbit during the Julian century.

    Parameters
    ----------
    jc: float
        the Julian century

    Returns
    -------
    float
    """
    return 0.016708634 - jc * (0.000042037 + 0.0000001267 * jc)


def sun_eq_of_ctr(jc, gmas):
    """
    The sun equation of center is the angular difference between the actual position of the sun with
    respect to the position of Earth, in its elliptical orbit and the position it would occupy if its motion were
    uniform, in a circular orbit of the same period.

    Parameters
    ----------
    jc: float
        the Julian century
    gmas: float
        the mean anomaly of the sun during the given Julian century

    Returns
    -------
    float
    """
    return np.deg2rad(np.sin(gmas) * (1.914602 - jc * (0.004817 + 0.000014 * jc)) +
                      np.sin(2 * gmas) * (0.019993 - 0.000101 * jc) +
                      np.sin(3 * gmas) * 0.000289)


def sun_true_long(gmls, seoc):
    """
    The true longitude of the sun.

    Parameters
    ----------
    gmls: float
        the mean longitude of the sun at the given Julian century
    seoc: float
        the equation of the center of the sun

    Returns
    -------
    float
    """
    return gmls + seoc


def sun_true_anom(gmas, seoc):
    """
    The true anomaly of the sun.

    Parameters
    ----------
    gmas: float
        the mean anomaly of the sun during the given Julian century
    seoc: float
        the equation of the center of the sun

    Returns
    -------
    float
    """
    return gmas + seoc


def sun_rad_vector(eeo, sta):
    """
    Sun radius vector is the distance from the sun to earth.

    Parameters
    ----------
    eeo: float
        inclination of the plane of the Earth's orbit during the Julian century
    sta: float
        the true anomaly of the sun

    Returns
    -------
    float
    """
    return (1.000001018 * (1 - np.square(eeo))) / (1 + eeo * np.cos(sta))


def sun_app_long(jc, stl):
    """
    The apparent longitude of the sun is the celestial longitude corrected for aberration and nutation as opposed
    to the mean longitude.

    Parameters
    ----------
    jc: float
        the Julian century
    stl: float
        the true longitude of the sun

    Returns
    -------
    float
    """
    return stl - np.deg2rad(0.00569 + 0.00478 * np.sin(np.deg2rad(125.04 - 1934.136 * jc)))


def mean_obliq_ecliptic(jc):
    """
    The mean obliquity of the ecliptic given the Julian century. The angle between the plane of the earth's orbit and
    the plane of the earth's equator; the "tilt" of the earth.

    Parameters
    ----------
    jc: float
        the Julian century

    Returns
    -------
    float
    """
    return np.deg2rad(23 + (26 + (21.448 - jc * (46.815 + jc * (0.00059 - jc * 0.001813))) / 60) / 60)


def obliq_corr(jc, moe):
    """
    The oblique correction refers to a particular type of the radiative corrections in the electroweak sector of the
    Standard model

    Parameters
    ----------
    jc: float
        the Julian century
    moe: float
        the mean obliquity of the ecliptic

    Returns
    -------
    float
    """
    return moe + np.deg2rad(0.00256) * np.cos(np.deg2rad(125.04 - 1934.136 * jc))


def sun_rt_ascen(sal, oc):
    """
    The right ascension of the sun. This is the angular distance of the sun measured eastward along the celestial
    equator from the North at the March equinox to the (hour circle of the) point in question above the earth.

    Parameters
    ----------
    sal: float
        the apparent longitude of the sun
    oc: float

    Returns
    -------
    float
    """
    return np.arctan2(np.cos(oc) * np.sin(sal), np.cos(sal))


def sun_declin(sal, oc):
    """
    The declination of the sun. This is the angle between the rays of the sun and the plane of the earth's equator.

    Parameters
    ----------
    sal: float
        the apparent longitude of the sun
    oc: float
        the oblique correction

    Returns
    -------
    float
    """
    return np.arcsin(np.sin(oc) * np.sin(sal))


def var_y(oc):
    """
    The var Y.

    Parameters
    ----------
    oc: float
        the oblique correction

    Returns
    -------
    float
    """
    return np.square(np.tan(oc / 2))


def eq_of_time(gmls, gmas, eeo, vy):
    """
    The equation of time. Describes the discrepancy between two kinds of solar time.

    Parameters
    ----------
    gmls: float
        the mean longitude of the sun at the given Julian century
    gmas: float
        the mean anomaly of the sun during the given Julian century
    eeo: float
        inclination of the plane of the Earth's orbit during the Julian century
    vy: float
        the var Y

    Returns
    -------
    float
    """
    return 4 * np.rad2deg(
        vy * np.sin(2 * gmls) -
        2 * eeo * np.sin(gmas) +
        4 * eeo * vy * np.sin(gmas) * np.cos(2 * gmls) -
        0.5 * np.square(vy) * np.sin(4 * gmls) - 1.25 * np.square(eeo) * np.sin(2 * gmas))


def ha_sunrise(lat, sd):
    """
    The sunrise hour angle.

    Parameters
    ----------
    lat: float
        the latitude of the observer
    sd: float
        the declination of the sun

    Returns
    -------
    float
    """
    return np.arccos(np.clip(np.cos(np.deg2rad(90.833)) / (np.cos(lat) * np.cos(sd)) - np.tan(lat) * np.tan(sd), -1, 1))


def solar_noon(lon, eot, tz=0):
    """
    The solar noon.

    Parameters
    ----------
    lon: float
        the longitude of the observer
    eot: float
        the equation of time
    tz: int
        the timezone (from GMT)

    Returns
    -------
    float
    """
    return (720 - 4 * np.rad2deg(lon) - eot + tz * 60) / 1440


def sunrise_time(hasr, sn):
    """
    The sunrise time.

    Parameters
    ----------
    hasr: float
        the sunrise hour angle
    sn: float
        the solar noon

    Returns
    -------
    float
    """
    return sn - np.rad2deg(hasr) * 4 / 1440


def sunset_time(hasr, sn):
    """
    The sunset time.

    Parameters
    ----------
    hasr: float
        the sunrise hour angle
    sn: float
        the solar noon

    Returns
    -------
    float
    """
    return sn + np.rad2deg(hasr) * 4 / 1440


def sunlight_duration(hasr):
    """
    The duration of the sunlight during the current day.

    Parameters
    ----------
    hasr: float
        the sunrise hour angle

    Returns
    -------
    float
    """
    return 8 * np.rad2deg(hasr)


def true_solar_time(lon, date, eot, tz=0):
    """
    The true solar time.

    Parameters
    ----------
    lon: float
        the longitude of the observer
    date: datetime
        the date and time of interest
    eot: float
        the equation of time
    tz: int
        the timezone (from GMT)

    Returns
    -------
    float
    """
    h = (date.hour + (date.minute + date.second / 60) / 60) / 24
    return (h * 1440 + eot + 4 * np.rad2deg(lon) - 60 * tz) % 1440


def hour_angle(tst):
    """
    The hour angle.

    Parameters
    ----------
    tst: float
        the true solar time

    Returns
    -------
    float
    """
    return np.deg2rad(tst / 4 + 180 if tst < 0 else tst / 4 - 180)
    # return np.deg2rad(tst / 4 + 180) % (2 * np.pi) - np.pi


def solar_zenith_angle(lat, sd, ha):
    """
    The solar zenith angle.

    Parameters
    ----------
    lat: float
        the latitude of the observer
    sd: float
        the declination of the sun
    ha: float
        the hour angle

    Returns
    -------
    float
    """
    return np.arccos(np.sin(lat) * np.sin(sd) + np.cos(lat) * np.cos(sd) * np.cos(ha))


def solar_elevation_angle(sza):
    """
    The solar elevation angle.

    Parameters
    ----------
    sza: float
        the solar zenith angle

    Returns
    -------
    float
    """
    return np.pi/2 - sza


def approx_atmospheric_refraction(sea):
    """
    The approximate atmospheric refraction.

    Parameters
    ----------
    sea: float
        the solar elevation angle

    Returns
    -------
    float
    """
    if np.rad2deg(sea) > 85:
        return 0
    elif np.rad2deg(sea) > 5:
        return np.deg2rad((1 / np.tan(sea) - 0.07 / np.power(np.tan(sea), 3) + 0.000086 / np.power(np.tan(sea), 5)) / 3600)
    elif np.rad2deg(sea) > -0.575:
        return np.deg2rad((1735 + sea * (-518.2 - sea * (-518.2 + sea * (103.4 + sea * (-12.79 + sea * 0.711))))) / 3600)
    else:
        return np.deg2rad((-20.772 / np.tan(sea)) / 3600)


def solar_elevation_corrected_for_atm_refraction(sea, aar):
    """
    The solar elevation corrected for the atmospheric refraction.

    Parameters
    ----------
    sea: float
        the solar elevation angle
    aar: float
        the approximate atmospheric refraction

    Returns
    -------
    float
    """
    return sea + aar


def solar_azimuth_angle(lat, ha, sza, sd):
    """
    The solar azimuth angle.

    Parameters
    ----------
    lat: float
        the latitude of the observer
    ha: float
        the hour angle
    sza: float
        the solar zenith angle
    sd: float
        the declination of the sun

    Returns
    -------
    float
    """
    temp = np.arccos(((np.sin(lat) * np.cos(sza)) - np.sin(sd)) / (np.cos(lat) * np.sin(sza)))
    if ha > 0:
        return (temp + np.pi) % (2 * np.pi)
    else:
        return (np.deg2rad(540) - temp) % (2 * np.pi)


def relative_to_absolute_time(obs, time):
    """
    Gets the data and timezone from an observer and overwrites its time based on the given time in days.

    Parameters
    ----------
    obs: Observer
        the observer that we take the date and timezone from
    time: float
        time in days

    Returns
    -------
    datetime
    """
    h = (time % 1) * 24
    m = (h - int(h)) * 60
    s = (m - int(m)) * 60
    return datetime(year=obs.date.year, month=obs.date.month, day=obs.date.day,
                    hour=int(h), minute=int(m), second=int(s), tzinfo=obs.timezone)

from scipy.spatial.transform import Rotation as R
# from datetime import datetime

# import numpy as np

T_L = np.array([[ 0.1787, -1.4630],
                [-0.3554,  0.4275],
                [-0.0227,  5.3251],
                [ 0.1206, -2.5771],
                [-0.0670,  0.3703]])
"""Transformation matrix of turbidity to luminance coefficients"""

"""
Now we have everything we need to make the Sky class
"""


class UniformSky(object):
    def __init__(self, luminance=1., name="uniform-sky"):
        self.__luminance = luminance

        self.__y = np.full(1, np.nan)
        self.__aop = np.full(1, np.nan)
        self.__dop = np.full(1, np.nan)
        self.__eta = np.full(1, False)
        self.__theta = np.full(1, np.nan)
        self.__phi = np.full(1, np.nan)

        self._is_generated = False
        self.__name = name

    def __call__(self, ori=None, irgbu=None, noise=0., eta=None, rng=RNG):
        """
        Generates the skylight properties for the given orientations and spectral influences.

        Parameters
        ----------
        ori: R, optional
            orientation of the interesting elements. Default is None
        irgbu: np.ndarray[float], optional
            the spectral influence of the observer
        noise: float, optional
            the noise level (sigma)
        eta: np.ndarray[float], optional
            :param eta: array of noise level in each point of interest
        rng
            the random generator

        Returns
        -------
        Y: np.ndarray[float]
            the luminance
        P: np.ndarray[float]
            the degree of polarisation
        A: np.ndarray[float]
            the angle of polarisation
        """

        # set default arguments
        self._update_coordinates(ori)

        # calculate light properties
        y = np.full_like(self.__phi, self.__luminance)  # Illumination
        p = np.full_like(self.__phi, 0.)  # Illumination
        a = np.full_like(self.__phi, np.nan)  # Illumination

        # create cloud disturbance
        if eta is None:
            eta = add_noise(noise=noise, shape=y.shape, rng=rng)
        y[eta] = 0.

        self.__y = y
        self.__dop = p
        self.__aop = a
        self.__eta = eta

        self._is_generated = True

        if irgbu is not None:
            y = spectrum_influence(y, irgbu).sum(axis=1)

        return y, p, a

    def _update_coordinates(self, ori=None):
        if ori is not None:
            xyz = np.clip(ori.apply([1, 0, 0]), -1, 1)
            phi = np.arctan2(xyz[..., 1], xyz[..., 0])
            theta = np.arccos(xyz[..., 2])
            # theta[xyz[..., 2] > 0] = np.pi - theta[xyz[..., 2] > 0]
            phi = (phi + np.pi) % (2 * np.pi) - np.pi

            # save points of interest
            self.theta = theta.copy()
            self.phi = phi.copy()
        else:
            ori = R.from_euler('ZY', np.vstack([self.phi, self.theta]).T, degrees=False)

        return ori

    @property
    def Y(self):
        """
        The luminance of the sky (K cd/m^2)

        Returns
        -------
        np.ndarray[float]
        """
        assert self._is_generated, "Sky is not generated yet. In order to generate the env, use the call function."
        return self.__y

    @property
    def DOP(self):
        """
        The linear degree of polarisation in the sky

        Returns
        -------
        np.ndarray[float]
        """
        assert self._is_generated, "Sky is not generated yet. In order to generate the env, use the call function."
        return self.__dop

    @property
    def AOP(self):
        """
        The angle of linear polarisation in the sky

        Returns
        -------
        np.ndarray[float]
        """
        assert self._is_generated, "Sky is not generated yet. In order to generate the env, use the call function."
        return self.__aop

    @property
    def theta(self):
        """
        The elevation of the last used elements.

        Returns
        -------
        np.ndarray[float]
        """
        assert self._is_generated, "Sky is not generated yet. In order to generate sky env, use the call function."
        return self.__theta

    @theta.setter
    def theta(self, value):
        self.__theta = value
        self._is_generated = False

    @property
    def phi(self):
        """
        The azimuth of the last used elements.

        Returns
        -------
        np.ndarray[float]
        """
        assert self._is_generated, "Sky is not generated yet. In order to generate the sky, use the call function."
        return self.__phi

    @phi.setter
    def phi(self, value):
        self.__phi = value
        self._is_generated = False

    @property
    def eta(self):
        """
        The percentage of noise induced in each element.

        Returns
        -------
        np.ndarray[float]
        """
        assert self._is_generated, "Sky is not generated yet. In order to generate the sky, use the call function."
        return self.__eta

    @eta.setter
    def eta(self, value):
        self.__eta = value
        self._is_generated = False

    @property
    def _y(self):
        return self.__y

    @_y.setter
    def _y(self, value):
        self.__y = value

    @property
    def _dop(self):
        return self.__dop

    @_dop.setter
    def _dop(self, value):
        self.__dop = value

    @property
    def _aop(self):
        return self.__aop

    @_aop.setter
    def _aop(self, value):
        self.__aop = value

    @property
    def _theta(self):
        return self.__theta

    @_theta.setter
    def _theta(self, value):
        self.__theta = value

    @property
    def _phi(self):
        return self.__phi

    @_phi.setter
    def _phi(self, value):
        self.__phi = value


class Sky(UniformSky):

    def __init__(self, theta_s=0., phi_s=0., degrees=False, name="sky"):
        """
        The Sky environment class. This environment class provides skylight cues.

        Parameters
        ----------
        theta_s: float, optional
            sun elevation (distance from horizon). Default is 0
        phi_s: float, optional
            sun azimuth (clockwise from North). Default is 0
        degrees: bool, optional
            True if the angles are given in degrees, False otherwise. Default is False
        name: str, optional
            a name for the sky instance. Default is 'sky'
        """
        super(Sky, self).__init__(name=name)
        self.__a, self.__b, self.__c, self.__d, self.__e = 0., 0., 0., 0., 0.
        self.__tau_L = 2.
        self._update_luminance_coefficients(self.__tau_L)
        self.__c1 = .6
        self.__c2 = 4.
        self.theta_s = np.deg2rad(theta_s) if degrees else theta_s
        self.phi_s = np.deg2rad(phi_s) if degrees else phi_s

    def __call__(self, ori=None, irgbu=None, noise=0., eta=None, rng=RNG):
        """
        Generates the skylight properties for the given orientations and spectral influences.

        Parameters
        ----------
        ori: R, optional
            orientation of the interesting elements. Default is None
        irgbu: np.ndarray[float], optional
            the spectral influence of the observer
        noise: float, optional
            the noise level (sigma)
        eta: np.ndarray[float], optional
            :param eta: array of noise level in each point of interest
        rng
            the random generator

        Returns
        -------
        Y: np.ndarray[float]
            the luminance
        P: np.ndarray[float]
            the degree of polarisation
        A: np.ndarray[float]
            the angle of polarisation
        """

        # set default arguments
        ori = self._update_coordinates(ori)
        theta = self._theta
        phi = self._phi

        theta_s, phi_s = self.theta_s, self.phi_s

        # SKY INTEGRATION
        gamma = np.arccos(np.cos(theta) * np.cos(theta_s) +
                          np.sin(theta) * np.sin(theta_s) * np.cos(phi - phi_s))

        # Intensity
        i_prez = self.L(gamma, theta)
        i_00 = self.L(0., theta_s)  # the luminance (Cd/m^2) at the zenith point
        i_90 = self.L(np.pi / 2, np.absolute(theta_s - np.pi / 2))  # the luminance (Cd/m^2) on the horizon
        # influence of env intensity
        i = (1. / (i_prez + eps) - 1. / (i_00 + eps)) * i_00 * i_90 / (i_00 - i_90 + eps)
        y = np.maximum(self.Y_z * i_prez / (i_00 + eps), 0.)  # Illumination

        # Degree of Polarisation
        lp = np.square(np.sin(gamma)) / (1 + np.square(np.cos(gamma)))
        p = np.clip(2. / np.pi * self.M_p * lp * (theta * np.cos(theta) + (np.pi / 2 - theta) * i), 0., 1.)

        # Angle of polarisation
        ori_s = R.from_euler('ZY', [phi_s, np.pi/2 - theta_s], degrees=False)
        x_s, y_s, _ = ori_s.apply([1, 0, 0]).T
        x_p, y_p, _ = ori.apply([1, 0, 0]).T
        a_x = np.arctan2(y_p - y_s, x_p - x_s) + np.pi/2
        a = (a_x + np.pi) % (2 * np.pi) - np.pi

        # create cloud disturbance
        if eta is None:
            eta = add_noise(noise=noise, shape=y.shape, rng=rng)
        y[eta] = 0.
        p[eta] = 0.  # destroy the polarisation pattern
        a[eta] = np.nan

        y[gamma < np.pi/60] = 17

        self._y = y
        self._dop = p
        self._aop = a
        self._eta = eta

        self._is_generated = True

        if irgbu is not None:
            y = spectrum_influence(y, irgbu).sum(axis=1)

        return y, p, a

    def L(self, chi, z):
        """
        Prez. et. al. Luminance function.
        Combines the scattering indicatrix and luminance gradation functions to compute the total
        luminance observed at the given env element(s).

        Parameters
        ----------
        chi: np.ndarray[float] | float
            angular distance between the observed element and the sun location -- [0, pi]
        z: np.ndarray[float]
            angular distance between the observed element and the zenith point -- [0, pi/2]

        Returns
        -------
        np.ndarray[float]
            the total observed luminance (Cd/m^2) at the given element(s)
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
        Darkening or brightening of the horizon

        Returns
        -------
        float
        """
        return self.__a

    @A.setter
    def A(self, value):
        self.__a = value
        self._update_turbidity(self.A, self.B, self.C, self.D, self.E)
        self._is_generated = False

    @property
    def B(self):
        """
        Luminance gradient near the horizon

        Returns
        -------
        float
        """
        return self.__b

    @B.setter
    def B(self, value):
        self.__b = value
        self._update_turbidity(self.A, self.B, self.C, self.D, self.E)
        self._is_generated = False

    @property
    def C(self):
        """
        Relative intensity of the circumsolar region

        Returns
        -------
        float
        """
        return self.__c

    @C.setter
    def C(self, value):
        self.__c = value
        self._update_turbidity(self.A, self.B, self.C, self.D, self.E)
        self._is_generated = False

    @property
    def D(self):
        """
        Width of the circumsolar region

        Returns
        -------
        float
        """
        return self.__d

    @D.setter
    def D(self, value):
        self.__d = value
        self._update_turbidity(self.A, self.B, self.C, self.D, self.E)
        self._is_generated = False

    @property
    def E(self):
        """
        Relative backscattered light

        Returns
        -------
        float
        """
        return self.__e

    @E.setter
    def E(self, value):
        self.__e = value
        self._update_turbidity(self.A, self.B, self.C, self.D, self.E)
        self._is_generated = False

    @property
    def c1(self):
        """
        1st coefficient of the maximum degree of polarisation

        Returns
        -------
        float
        """
        return self.__c1

    @property
    def c2(self):
        """
        2nd coefficient of the maximum degree of polarisation

        Returns
        -------
        float
        """
        return self.__c2

    @property
    def tau_L(self):
        """
        The atmospheric turbidity

        Returns
        -------
        float
        """
        return self.__tau_L

    @tau_L.setter
    def tau_L(self, value):
        assert value >= 1., "Turbidity must be greater or eaqual to 1."
        self._is_generated = self.__tau_L == value and self._is_generated
        self._update_luminance_coefficients(value)

    @property
    def Y_z(self):
        """
        The zenith luminance (K cd/m^2)

        Returns
        -------
        float
        """
        chi = (4. / 9. - self.tau_L / 120.) * (np.pi - 2 * (np.pi/2 - self.theta_s))
        return (4.0453 * self.tau_L - 4.9710) * np.tan(chi) - 0.2155 * self.tau_L + 2.4192

    @property
    def M_p(self):
        """
        Maximum degree of polarisation

        Returns
        -------
        float
        """
        return np.exp(-(self.tau_L - self.c1) / (self.c2 + eps))

    def _update_luminance_coefficients(self, tau_L):
        """
        Updates the luminance coefficients given the atmospheric turbidity.

        Parameters
        ----------
        tau_L: float
            the atmospheric turbidity
        """
        self.__a, self.__b, self.__c, self.__d, self.__e = T_L.dot(np.array([tau_L, 1.]))
        self._update_turbidity(self.A, self.B, self.C, self.D, self.E)

    def _update_turbidity(self, a, b, c, d, e):
        """
        Updates the atmospheric turbidity given the luminance coefficients.

        Parameters
        ----------
        a: float
            the darkening or brightening of horizon
        b: float
            the luminance gradient near the horizon
        c: float
            the relative intensity of the circumsolar region
        d: float
            the width of the circumsolar region
        e: float
            the relative backscattered light
        """
        T_T = np.linalg.pinv(T_L)
        tau_L, c = T_T.dot(np.array([a, b, c, d, e]))
        self.__tau_L = tau_L / c  # turbidity correction

    def copy(self):
        """
        Generates a copy of the instance.

        Returns
        -------
        Sky
        """
        sky = Sky()
        sky.tau_L = self.tau_L
        sky.theta_s = self.theta_s
        sky.phi_s = self.phi_s
        sky.__c1 = self.__c1
        sky.__c2 = self.__c2

        sky.__theta = self.__theta
        sky.__phi = self.__phi
        sky.__aop = self.__aop
        sky.__dop = self.__dop
        sky.__y = self.__y
        sky.__eta = self.__eta

        sky._is_generated = False
        return sky

    @staticmethod
    def from_observer(obs=None, date=None, ori=None):
        """
        Creates a sky instance using an observer on Earth.

        Parameters
        ----------
        obs: Observer, optional
            the observer (location on Earth). Default is the Seville observer
        date: datetime, optional
            the date of the observation. Default is 21/06/2021 - 10:00 am
        ori: R, optioanl
            the heading orientation of the observer. Default is 0.

        Returns
        -------
        Sky
        """

        sun = Sun()
        if obs is None:
            obs = get_seville_observer()
            obs.date = datetime(2021, 6, 21, 10, 0, 0) if date is None else date
        sun.compute(obs)
        if ori is not None:
            yaw, pitch, roll = ori.as_euler('ZYX', degrees=False)
        else:
            yaw = 0.
        theta_s, phi_s = np.pi/2 - sun.alt, (sun.az - yaw + np.pi) % (2 * np.pi) - np.pi

        return Sky(theta_s=theta_s, phi_s=phi_s)

    @staticmethod
    def from_type(sky_type):
        """
        Creates a sky model using a type description.

        - 1: Steep luminance gradation towards zenith, azimuthal uniformity
        - 2: Overcast, with steep luminance gradation and slight brightening towards the sun
        - 3: Overcast, moderately graded with azimuthal uniformity
        - 4: Overcast, moderately graded and slightly brightening towards the sun
        - 5: Sky uniform luminance
        - 6: Partly cloudy agent_old, no gradation towards zenith, slight brighening towards the sun
        - 7: Partly cloudy agent_old, no gradation towards zenith, brighter circumsolar region
        - 8: Partly cloudy agent_old, no gradation towards zenith, distinct solar corona
        - 9: Partly cloudy, with the obscured sun
        - 10: Partly cloudy, with brighter circumsolar region
        - 11: White-blue agent_old with distinct solar corona
        - 12: CIE Standard Clear Sky, low illuminance turbidity
        - 13: CIE Standard Clear Sky, polluted atmosphere
        - 14: Cloudless turbid agent_old with broad solar corona
        - 15: White-blue turbid agent_old with broad solar corona

        Parameters
        ----------
        sky_type: int
            a number in range [1-15] identifying the type of the sky

        Returns
        -------
        Sky
        """
        import os
        import yaml

        sp = os.path.join(__root__, 'data', 'standard-parameters.yaml')
        with open(sp, 'r') as f:
            try:
                sp = yaml.load(f)
            except yaml.YAMLError as exc:
                print("Could not load the env types.", exc)
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



def spectrum_influence(v, irgbu):
    """
    Decomposes the luminance into 5 distinct spectral channels based on the sensitivity provided.

    Parameters
    ----------
    v: np.ndarray[float]
        received luminance (white light)
    irgbu: np.ndarray[float]
        array of sensitivities for each channel (IR, R, G, B, UV)

    Returns
    -------
    np.ndarray[float]
        the luminance received in each channel
    """
    wl = np.array([1200, 715, 535, 475, 350], dtype='float32')
    v = v[..., np.newaxis]
    irgbu = np.vstack([irgbu] + [irgbu for _ in range(v.shape[0] // irgbu.shape[0] - 1)])

    l1 = 10.0 * irgbu * np.power(wl / 1000., 8) * np.square(v) / float(v.size)
    l2 = 0.001 * irgbu * np.power(1000. / wl, 8) * np.nansum(np.square(v)) / float(v.size)

    v_max = np.nanmax(v)
    w_max = np.nanmax(v + l1 + l2)
    w = v_max * (v + l1 + l2) / w_max
    if isinstance(irgbu, np.ndarray):
        if irgbu.shape[0] == 1 and w.shape[0] > irgbu.shape[0]:
            irgbu = np.vstack([irgbu] * w.shape[0])
        w[irgbu < 0] = np.hstack([v] * irgbu.shape[1])[irgbu < 0]
    elif irgbu < 0:
        w = v
    return w


def visualise_luminance(sky):
    """
    Plots the sky luminance.

    Parameters
    ----------
    sky: Sky
        the sky model
    """
    import matplotlib.pyplot as plt

    plt.figure("Luminance", figsize=(4.5, 4.5))
    ax = plt.subplot(111, polar=True)
    ax.set_theta_zero_location("N")
    ax.set_theta_direction(-1)

    theta_s, phi_s = sky.theta_s, sky.phi_s
    ax.scatter(sky.phi, sky.theta, s=20, c=sky.Y, marker='.', cmap='Blues_r', vmin=0, vmax=6)
    ax.scatter(phi_s, theta_s, s=100, edgecolor='black', facecolor='yellow')
    # ax.scatter(env.phi_t + np.pi, env.theta_t, s=200, edgecolor='black', facecolor='greenyellow')
    ax.set_ylim([0, np.pi/2])
    ax.set_yticks([])
    ax.set_xticks(np.linspace(0, 2*np.pi, 8, endpoint=False))
    ax.set_xticklabels([r'$0^\circ$ (N)', r'$45^\circ$ (NE)', r'$90^\circ$ (E)', r'$135^\circ$ (SE)',
                        r'$180^\circ$ (S)', r'$-135^\circ$ (SW)', r'$-90^\circ$ (W)', r'$-45^\circ$ (NW)'])

    plt.show()


def visualise_degree_of_polarisation(sky):
    """
    Plots the degree of polarisation in the sky.

    Parameters
    ----------
    sky: Sky
        the sky model
    """
    import matplotlib.pyplot as plt

    plt.figure("degree-of-polarisation", figsize=(4.5, 4.5))
    ax = plt.subplot(111, polar=True)
    ax.set_theta_zero_location("N")
    ax.set_theta_direction(-1)

    theta_s, phi_s = sky.theta_s, sky.phi_s
    print(theta_s, phi_s)
    ax.scatter(sky.phi, sky.theta, s=10, c=sky.DOP, marker='.', cmap='Greys', vmin=0, vmax=1)
    ax.scatter(phi_s, theta_s, s=100, edgecolor='black', facecolor='yellow')
    # ax.scatter(env.phi_t + np.pi, env.theta_t, s=200, edgecolor='black', facecolor='greenyellow')
    ax.set_ylim([0, np.pi/2])
    ax.set_yticks([])
    ax.set_xticks(np.linspace(0, 2*np.pi, 8, endpoint=False))
    ax.set_xticklabels([r'$0^\circ$ (N)', r'$45^\circ$ (NE)', r'$90^\circ$ (E)', r'$135^\circ$ (SE)',
                        r'$180^\circ$ (S)', r'$-135^\circ$ (SW)', r'$-90^\circ$ (W)', r'$-45^\circ$ (NW)'])

    plt.show()


def visualise_angle_of_polarisation(sky):
    """
    Plots the angle of polarisation in the sky.

    Parameters
    ----------
    sky: Sky
        the sky model
    """
    import matplotlib.pyplot as plt

    plt.figure("angle-of-polarisation", figsize=(4.5, 4.5))
    ax = plt.subplot(111, polar=True)
    ax.set_theta_zero_location("N")
    ax.set_theta_direction(-1)

    theta_s, phi_s = sky.theta_s, sky.phi_s
    ax.scatter(sky.phi, sky.theta, s=10, c=sky.AOP, marker='.', cmap='hsv', vmin=-np.pi, vmax=np.pi)
    ax.scatter(phi_s, theta_s, s=100, edgecolor='black', facecolor='yellow')
    # ax.scatter(env.phi_t + np.pi, env.theta_t, s=200, edgecolor='black', facecolor='greenyellow')
    ax.set_ylim([0, np.pi/2])
    ax.set_yticks([])
    ax.set_xticks(np.linspace(0, 2*np.pi, 8, endpoint=False))
    ax.set_xticklabels([r'$0^\circ$ (N)', r'$45^\circ$ (NE)', r'$90^\circ$ (E)', r'$135^\circ$ (SE)',
                        r'$180^\circ$ (S)', r'$-135^\circ$ (SW)', r'$-90^\circ$ (W)', r'$-45^\circ$ (NW)'])

    plt.show()



"""
Test it out!
"""
from invertpy.sense._helpers import fibonacci_sphere
from invertpy.sense.vision import CompoundEye

# from invertsy.env.sky import Sky
#WHERE IS invertsy.simplot._plots?!!!
# from invertsy.simplot._plots import plot_sky


# from scipy.spatial.transform import Rotation as R

# import numpy as np
import matplotlib.pyplot as plt


flat = True
samples = 5000

phi, theta, _ = fibonacci_sphere(samples, np.pi).T
ori = R.from_euler('ZY', np.vstack([phi, theta]).T, degrees=False)
sky = Sky(np.deg2rad(60), np.pi)
# sky = Sky(np.deg2rad(5), np.pi)

eye = CompoundEye(omm_ori=ori, omm_rho=np.deg2rad(5),
                  ori=R.from_euler('ZYX', [0, 0, 0], degrees=True),
                  omm_pol_op=0., noise=0.)
r = eye(sky=sky)
print(eye)

oris = eye.ori * eye.omm_ori
y, p, a = sky(oris)
y = np.square(r[..., 1])
# print(r.min(), r.max())

plt.figure("env", figsize=(10, 3.33))
# plot_sky(eye.omm_ori, y, p, a, flat=flat).show()

"""try this out instead"""
visualise_angle_of_polarisation(sky)
visualise_degree_of_polarisation(sky)
visualise_luminance(sky)

"""Now with a bee eye"""
runfile('C:/Users/jaf54iq/src/InvertPy/' +'/examples/create_bee_eye.py', wdir = 'C:/Users/jaf54iq/src/InvertPy/' )
bre = build_right_bee_eye()
rr = bre(sky = sky)
visualise_angle_of_polarisation(sky)
visualise_degree_of_polarisation(sky)
visualise_luminance(sky)

"""When the eye sees the sky this happens"""
bre_oris = bre.ori * bre.omm_ori # these are spatial rotation object that interact
y, p, a = sky(bre_oris)

np.sum(bre.omm_pol) # I am only interested in these ommatidia
np.sum(bre.omm_pol<1) # not these ones

"""Build bee DRA"""
def build_right_bee_DRA():
    """
    Following [1]_

    Notes
    -----
    .. [1] "Mimicking honeybee eyes with a 280 degrees field of view catadioptric imaging system"
            http://iopscience.iop.org/article/10.1088/1748-3182/5/3/036002/pdf
    """
    Delta_phi_min_v = np.deg2rad(1.5)
    Delta_phi_max_v = np.deg2rad(4.5)
    Delta_phi_min_h = np.deg2rad(2.4)
    Delta_phi_mid_h = np.deg2rad(3.7)
    Delta_phi_max_h = np.deg2rad(4.6)

    def norm_alpha(alpha):
        alpha = alpha % (2 * np.pi)
        if alpha > 3 * np.pi / 2:
            alpha -= 2 * np.pi
        return alpha

    def norm_epsilon(epsilon):
        epsilon = epsilon % (2 * np.pi)
        if epsilon > np.pi:
            epsilon -= 2 * np.pi
        return epsilon

    def xi(z=None, alpha=None, epsilon=None):
        if alpha is not None:
            if z in [1, 2]:  # or 0 <= alpha <= 3 * np.pi / 2:
                return 1.
            if z in [3, 4]:  # or 3 * np.pi / 2 <= alpha:
                return -1.
        if epsilon is not None:
            if z in [1, 4]:  # or 0 <= epsilon <= np.pi / 2:
                return 1.
            if z in [2, 3]:  # or 3 * np.pi / 2 <= epsilon:
                return -1.
        return 0.

    def Delta_alpha(Delta_phi_h, epsilon):
        return norm_alpha((2 * np.arcsin(np.sin(Delta_phi_h / 2.) / np.cos(epsilon))) % (2 * np.pi))

    def phi_h(z, alpha, epsilon):
        alpha = norm_alpha(alpha)
        epsilon = norm_epsilon(epsilon)
        abse = np.absolute(epsilon)
        absa = np.absolute(alpha)

        # zone Z=1/2
        if z in [1, 2]:
            if 0 <= alpha <= np.pi / 4:
                return Delta_phi_mid_h + alpha / (np.pi / 4) * (Delta_phi_min_h - Delta_phi_mid_h)
            if np.pi / 4 < alpha <= np.pi / 2:
                return Delta_phi_min_h + (alpha - np.pi / 4) / (np.pi / 4) * (Delta_phi_mid_h - Delta_phi_min_h)
            if np.pi / 2 < alpha <= np.deg2rad(150) and abse <= np.deg2rad(50):
                return Delta_phi_mid_h + (alpha - np.pi / 2) / (np.pi / 3) * (Delta_phi_max_h - Delta_phi_mid_h)
            if np.pi / 2 < alpha <= np.deg2rad(150) and abse > np.deg2rad(50):
                return Delta_phi_mid_h + (alpha - np.pi / 2) / (np.pi / 3) * (Delta_phi_max_h - Delta_phi_mid_h) \
                                             * (np.pi / 2 - abse) / np.deg2rad(40)
            if np.deg2rad(150) < alpha <= np.pi and abse <= np.deg2rad(50):
                return Delta_phi_max_h
            if np.deg2rad(150) < alpha <= np.pi and abse > np.deg2rad(50):
                return Delta_phi_mid_h + (Delta_phi_max_h - Delta_phi_mid_h) * (np.pi / 2 - abse) / np.deg2rad(40)
            if np.pi < alpha <= 3 * np.pi / 2:
                return Delta_phi_max_h

        if z in [3, 4]:
            # zone Z=3
            if -np.pi / 2 <= alpha < -np.pi / 4 and epsilon <= -np.deg2rad(50):
                return Delta_phi_mid_h + (absa - np.pi / 4) / (np.pi / 4) * (Delta_phi_max_h - Delta_phi_mid_h) \
                                         * (np.pi / 2 - abse) / np.deg2rad(40)
            if -np.pi / 4 <= alpha <= 0 and epsilon <= -np.deg2rad(50):
                return Delta_phi_mid_h
            if -np.pi / 4 < alpha <= 0 and -np.deg2rad(50) < epsilon <= 0:
                return Delta_phi_mid_h + absa / (np.pi / 4) * (Delta_phi_max_h - Delta_phi_mid_h) \
                                         * (np.deg2rad(50) - abse) / np.deg2rad(50)

            # zone Z=4
            if -np.pi / 2 <= alpha <= -np.pi / 4 and epsilon >= np.deg2rad(50):
                return Delta_phi_max_h + (Delta_phi_max_h - Delta_phi_mid_h) * (np.pi / 2 - abse) / np.deg2rad(40)
            if -np.pi / 4 <= alpha <= 0 and 0 < epsilon <= np.deg2rad(50):
                return Delta_phi_mid_h + absa / (np.pi / 4) * (Delta_phi_max_h - Delta_phi_mid_h)
            if -np.pi / 4 <= alpha <= 0 and epsilon >= np.deg2rad(50):
                return Delta_phi_mid_h + absa / (np.pi / 4) * (Delta_phi_max_h - Delta_phi_mid_h) \
                                         * (np.pi / 2 - epsilon) / np.deg2rad(40)
        return np.nan

    def alpha_max(z, epsilon):
        if z in [1, 2]:
            return splev(epsilon, tck_1_2, ext=3, der=0)
        if z in [3, 4]:
            return splev(epsilon, tck_3_4, ext=3, der=0)
        return np.nan

    ommatidia = []
    # only zones 2 and 3
    zns = [2,3]
    for z in zns: #range(1, 5):
        j = 0
        epsilon = np.pi/3# start at edge of DRA 0.  # elevation
        alpha = 0.  # azimuth
        while np.absolute(epsilon) <= np.pi / 2:
            if j % 2 == 0:
                alpha = 0
            else:
                alpha = Delta_alpha(xi(z=z, alpha=alpha) * Delta_phi_mid_h / 2, epsilon)
            while np.absolute(alpha) < np.absolute(alpha_max(z, epsilon)):
                # print z, np.rad2deg(epsilon), np.rad2deg(alpha)
                Delta_phi_h = phi_h(z, alpha, epsilon)
                if np.isnan(Delta_phi_h):
                    break
                ommatidia.append(np.array([alpha, epsilon]))
                alpha = norm_alpha(alpha + Delta_alpha(xi(z=z, alpha=alpha) * Delta_phi_h, epsilon))
            Delta_phi_v = Delta_phi_min_v + (Delta_phi_max_v - Delta_phi_min_v) * np.absolute(epsilon) / (np.pi / 2)
            epsilon = norm_epsilon(epsilon + xi(z=z, epsilon=epsilon) * Delta_phi_v / 2.)
            j += 1

    ommatidia = np.array(ommatidia)
    ommatidia = ommatidia[ommatidia[...,1]< -np.pi/3,... ]
    omm_ori = R.from_euler('ZY', ommatidia)
    omm_rho = np.deg2rad(5) * (1 + ((np.pi/2 - ommatidia[:, 1]) % (2 * np.pi)) / np.pi)
    omm_pol = np.asarray(ommatidia[:, 1] > np.pi/3, dtype=float)
    spectral = (omm_pol[..., np.newaxis] * np.array([[0, 0, 0, 0, 1]], dtype=float) +
                (1 - omm_pol)[..., np.newaxis] * np.array([[0, 0, 1, 0, 0]], dtype=float))

    return CompoundEye(omm_ori=omm_ori, omm_rho=omm_rho, omm_pol_op=omm_pol, c_sensitive=spectral, name='bee_right_eye')


bdra = build_right_bee_DRA()
rrr = bdra(sky)
visualise_luminance(sky)
# ax = plt.gca()
# ax.set_ylim([np.pi/6, np.pi/2])
visualise_angle_of_polarisation(sky)
visualise_degree_of_polarisation(sky)


