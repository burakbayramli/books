
# [ATM 623: Climate Modeling](../index.ipynb)

[Brian E. J. Rose](http://www.atmos.albany.edu/facstaff/brose/index.html), University at Albany

# Lecture 5: A Brief Review of Radiation

### About these notes:

This document uses the interactive [`Jupyter notebook`](https://jupyter.org) format. The notes can be accessed in several different ways:

- The interactive notebooks are hosted on `github` at https://github.com/brian-rose/ClimateModeling_courseware
- The latest versions can be viewed as static web pages [rendered on nbviewer](http://nbviewer.ipython.org/github/brian-rose/ClimateModeling_courseware/blob/master/index.ipynb)
- A complete snapshot of the notes as of May 2017 (end of spring semester) are [available on Brian's website](http://www.atmos.albany.edu/facstaff/brose/classes/ATM623_Spring2017/Notes/index.html).

[Also here is a legacy version from 2015](http://www.atmos.albany.edu/facstaff/brose/classes/ATM623_Spring2015/Notes/index.html).

Many of these notes make use of the `climlab` package, available at https://github.com/brian-rose/climlab

## Contents

1. [Emission temperature and lapse rates](#section1)
2. [Solar Radiation](#section2)
3. [Terrestrial Radiation and absorption spectra](#section3)

____________
<a id='section1'></a>

## 1. Emission temperature and lapse rates
____________

Planetary energy balance is the foundation for all climate modeling. So far we have expressed this through a globally averaged budget

$$C \frac{d T_s}{dt} = (1-\alpha) Q - OLR$$

and we have written the OLR in terms of an emission temperature $T_e$ where by definition

$$ OLR = \sigma T_e^4 $$

Using values from the observed planetary energy budget, we found that $T_e = 255$ K

The emission temperature of the planet is thus about 33 K colder than the mean surface temperature (288 K).

### Where in the atmosphere do we find $T = T_e = 255$ K?

Let's get plot air temperature from NCEP reanalysis data.


```python
%matplotlib inline
import numpy as np
import matplotlib.pyplot as plt
import xarray as xr
#  Disable interactive plotting (use explicit display calls to show figures)
plt.ioff()
```


```python
ncep_url = "http://www.esrl.noaa.gov/psd/thredds/dodsC/Datasets/ncep.reanalysis.derived/"
ncep_air = xr.open_dataset( ncep_url + "pressure/air.mon.1981-2010.ltm.nc",
                          decode_times=False)
print ncep_air
```

    <xarray.Dataset>
    Dimensions:             (lat: 73, level: 17, lon: 144, nbnds: 2, time: 12)
    Coordinates:
      * level               (level) float32 1000.0 925.0 850.0 700.0 600.0 500.0 ...
      * lon                 (lon) float32 0.0 2.5 5.0 7.5 10.0 12.5 15.0 17.5 ...
      * time                (time) float64 -6.571e+05 -6.57e+05 -6.57e+05 ...
      * lat                 (lat) float32 90.0 87.5 85.0 82.5 80.0 77.5 75.0 ...
    Dimensions without coordinates: nbnds
    Data variables:
        climatology_bounds  (time, nbnds) float64 ...
        air                 (time, level, lat, lon) float64 ...
        valid_yr_count      (time, level, lat, lon) float64 ...
    Attributes:
        description:                     Data from NCEP initialized reanalysis (4...
        platform:                       Model
        Conventions:                    COARDS
        not_missing_threshold_percent:  minimum 3% values input to have non-missi...
        history:                        Created 2011/07/12 by doMonthLTM\nConvert...
        title:                          monthly ltm air from the NCEP Reanalysis
        References:                     http://www.esrl.noaa.gov/psd/data/gridded...
        dataset_title:                  NCEP-NCAR Reanalysis 1



```python
#  Take global, annual average and convert to Kelvin
from xarray.ufuncs import cos, deg2rad, log
tempCtoK = 273.15
coslat = cos(deg2rad(ncep_air.lat))
weight = coslat / coslat.mean(dim='lat')
Tglobal = (ncep_air.air * weight).mean(dim=('lat','lon','time'))
Tglobal += tempCtoK
print Tglobal
```

    <xarray.DataArray (level: 17)>
    array([ 288.329082,  284.357002,  280.988327,  273.369941,  266.701657,
            258.261156,  247.579533,  233.780315,  226.352092,  219.497765,
            212.586449,  206.143952,  207.617073,  211.663363,  217.296419,
            221.556055,  229.930018])
    Coordinates:
      * level    (level) float32 1000.0 925.0 850.0 700.0 600.0 500.0 400.0 ...



```python
# a logarithmic pressure axis
#  Approximate geometrical height based on pressure scale height of 8 km
scale_height_km = 8.
zstar = -scale_height_km * log(ncep_air.level/1000.)
print zstar
```

    <xarray.DataArray 'level' (level: 17)>
    array([ -0.      ,   0.623692,   1.300151,   2.8534  ,   4.086605,   5.545177,
             7.330326,   9.631783,  11.090355,  12.875504,  15.17696 ,  18.420681,
            21.27408 ,  23.965858,  28.052464,  31.296185,  36.841362], dtype=float32)
    Coordinates:
      * level    (level) float32 1000.0 925.0 850.0 700.0 600.0 500.0 400.0 ...



```python
label_size = 16
fig, ax = plt.subplots( figsize=(10,8) )
ax.plot( Tglobal, zstar )

ax.set_xlabel('Temperature (K)', fontsize=label_size)
ax.set_ylabel('Pressure (hPa)', fontsize=label_size)
ax.set_yticks(zstar)
# Label the axis with pressure values
ax.set_yticklabels(zstar.level.values)
ax.set_title('Global, annual mean sounding from NCEP Reanalysis', 
             fontsize = 24)
#  Create a second y axis on the right side, label with approx. geometric height
ax2 = ax.twinx()
ax2.plot(Tglobal, zstar);
ax2.set_ylabel('Approx. height above surface (km)', 
               fontsize=label_size);
ax.grid()
```


```python
#  See code above for details
fig
```




![png](output_12_0.png)



Note that surface temperature in global mean is indeed about 288 K as we keep saying.

So where do we find temperature $T_e=255$ K?

Actually in mid-troposphere, near 500 hPa or about 5 km height.

We can infer that much of the outgoing longwave radiation actually originates far above the surface.

Recall that our observed global energy budget diagram shows 217 out of 239 W m$^{-2}$ total OLR emitted by the atmosphere and clouds, only 22 W m$^{-2}$ directly from the surface.

This is due to the **greenhouse effect**. 

So far we have dealt with the greenhouse in a very artificial way in our energy balance model by simply assuming a linear relationship 

$$ T_e = \beta T_s$$

which is equivalent to assuming the OLR is reduced by a constant factor from the value it would have if the Earth emitted as a blackbody at the surface temperature.

Now it's time to start thinking a bit more about how the radiative transfer process actually occurs in the atmosphere, and how to model it.

____________
<a id='section2'></a>

## 2. Solar Radiation
____________

Let's plot a spectrum of solar radiation.

For details, see code in notebook!


```python
#  Using pre-defined code for the Planck function from the climlab package
from climlab.utils.thermo import Planck_wavelength
```


```python
# approximate emission temperature of the sun in Kelvin
Tsun = 5780.
# boundaries of visible region in nanometers
UVbound = 390.
IRbound = 700.
#  array of wavelengths
wavelength_nm = np.linspace(0., 3500, 400)
to_meters = 1E-9  # conversion factor
```


```python
fig, ax = plt.subplots(figsize=(14,7))
ax.plot(wavelength_nm, 
         Planck_wavelength(wavelength_nm * to_meters, Tsun))
ax.grid()
ax.set_xlabel('Wavelength (nm)', fontsize=label_size)
ax.set_ylabel('Spectral radiance (W sr$^{-1}$ m$^{-3}$)', fontsize=label_size)
#  Mask out points outside of this range
wavelength_vis = np.ma.masked_outside(wavelength_nm, UVbound, IRbound)
#  Shade the visible region
ax.fill_between(wavelength_vis, Planck_wavelength(wavelength_vis * to_meters, Tsun))
title = 'Blackbody emission curve for the sun (T = {:.0f} K)'.format(Tsun)
ax.set_title(title, fontsize=label_size);
ax.text(280, 0.8E13, 'Ultraviolet', rotation='vertical', fontsize=12)
ax.text(500, 0.8E13, 'Visible', rotation='vertical', fontsize=16, color='w')
ax.text(800, 0.8E13, 'Infrared', rotation='vertical', fontsize=12);
```


```python
# see code above for details
fig
```




![png](output_20_0.png)



- Spectrum peaks in the visible range
- most energy at these wavelength.
- No coincidence that our eyes are sensitive to this range of wavelengths!
- Longer wavelengths called “infrared”, shorter wavelengths called “ultraviolet”.

The shape of the spectrum is a fundamental characteristic of radiative emissions
(think about the color of burning coals in a fire – cooler = red, hotter = white)

Theory and experiments tell us that both the total flux of emitted radiation, and the wavelength of maximum emission, depend only on the temperature of the source!

The theoretical spectrum was worked out by Max Planck and is therefore known as the “Planck” spectrum (or simply blackbody spectrum).


```python
from IPython.display import Image
Image('../images/MarshallPlumbFig2.3.png')
```




![png](output_23_0.png)



*Figure reproduced from Marshall and Plumb (2008): Atmosphere, Ocean, and Climate Dynamics*

Going from cool to warm:

- total emission increases
- maximum emission occurs at shorter wavelengths.

The **integral of these curves over all wavelengths** gives us our familiar $\sigma T^4$

Mathematically it turns out that 

$$ λ_{max} T = \text{constant} $$

(known as Wien’s displacement law).

By fitting the observed solar emission to a blackbody curve, we can deduce that the emission temperature of the sun is about 6000 K.

Knowing this, and knowing that the solar spectrum peaks at 0.6 micrometers, we can calculate the wavelength of maximum terrestrial radiation as

$$ λ_{max}^{Earth} = 0.6 ~ \mu m \frac{6000}{255} = 14 ~ \mu m $$

This is in the far-infrared part of the spectrum.

____________
<a id='section3'></a>

## 3. Terrestrial Radiation and absorption spectra
____________

Now let's look at normalized blackbody curves for Sun and Earth:


```python
Image('../images/MarshallPlumbFig2.5.png')
```




![png](output_29_0.png)



*Figure reproduced from Marshall and Plumb (2008): Atmosphere, Ocean, and Climate Dynamics*

There is essentially no overlap between the two spectra.

**This is the fundamental reason we can discuss the solar “shortwave” and terrestrial “longwave” radiation as two distinct phenomena.**

In reality all radiation exists on a continuum of different wavelengths. But in climate science we can get a long way by thinking in terms of a very simple “two-stream” approximation (short and longwave). We’ve already been doing this throughout the course so far!

Now look at the atmospheric **absorption spectra**.

(fraction of radiation at each wavelength that is absorbed on a single vertical path through the atmosphere)

- Atmosphere is almost completely transparent in the visible range, right at the peak of the solar spectrum
- Atmosphere is very opaque in the UV
- Opacity across the IR spectrum is highly variable!
- Look at the gases associated with various absorption features:
- Main players include H$_2$O, CO$_2$, N$_2$O, O$_2$.
- Compare to major constituents of atmosphere, in decreasing order: 
    - 78% N$_2$
    - 21% O$_2$
    - 1% Ar
    - H$_2$O (variable) 

- The dominant constituent gases N$_2$ and O$_2$ are nearly completely transparent across the entire spectrum (there are O$_2$ absorption features in far UV, but little energy at these wavelengths).
- The greenhouse effect mostly involves trace constituents: 
    - O$_3$ = 500 ppb
    - N$_2$O = 310 ppb
    - CO$_2$ = 400 ppm (but rapidly increasing!)
    - CH$_4$ = 1.7 ppm
- Note that most of these are tri-atomic molecules! There are fundamental reasons for this: these molecules have modes of rotational and vibration that are easily excited at IR wavelengths. See courses in radiative transfer!


<div class="alert alert-success">
[Back to ATM 623 notebook home](../index.ipynb)
</div>

____________
## Version information
____________


```python
%load_ext version_information
%version_information numpy, matplotlib, xarray, climlab
```




<table><tr><th>Software</th><th>Version</th></tr><tr><td>Python</td><td>2.7.12 64bit [GCC 4.2.1 (Based on Apple Inc. build 5658) (LLVM build 2336.11.00)]</td></tr><tr><td>IPython</td><td>5.3.0</td></tr><tr><td>OS</td><td>Darwin 16.5.0 x86_64 i386 64bit</td></tr><tr><td>numpy</td><td>1.11.1</td></tr><tr><td>matplotlib</td><td>2.0.0</td></tr><tr><td>xarray</td><td>0.9.5</td></tr><tr><td>climlab</td><td>0.5.6</td></tr><tr><td colspan='2'>Thu May 25 13:45:48 2017 EDT</td></tr></table>



____________

## Credits

The author of this notebook is [Brian E. J. Rose](http://www.atmos.albany.edu/facstaff/brose/index.html), University at Albany.

It was developed in support of [ATM 623: Climate Modeling](http://www.atmos.albany.edu/facstaff/brose/classes/ATM623_Spring2015/), a graduate-level course in the [Department of Atmospheric and Envionmental Sciences](http://www.albany.edu/atmos/index.php)

Development of these notes and the [climlab software](https://github.com/brian-rose/climlab) is partially supported by the National Science Foundation under award AGS-1455071 to Brian Rose. Any opinions, findings, conclusions or recommendations expressed here are mine and do not necessarily reflect the views of the National Science Foundation.
____________


```python

```
