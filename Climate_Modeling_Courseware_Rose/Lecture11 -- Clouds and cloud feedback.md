
# [ATM 623: Climate Modeling](../index.ipynb)

[Brian E. J. Rose](http://www.atmos.albany.edu/facstaff/brose/index.html), University at Albany

# Lecture 11: Clouds and cloud feedback

### About these notes:

This document uses the interactive [`Jupyter notebook`](https://jupyter.org) format. The notes can be accessed in several different ways:

- The interactive notebooks are hosted on `github` at https://github.com/brian-rose/ClimateModeling_courseware
- The latest versions can be viewed as static web pages [rendered on nbviewer](http://nbviewer.ipython.org/github/brian-rose/ClimateModeling_courseware/blob/master/index.ipynb)
- A complete snapshot of the notes as of May 2017 (end of spring semester) are [available on Brian's website](http://www.atmos.albany.edu/facstaff/brose/classes/ATM623_Spring2017/Notes/index.html).

[Also here is a legacy version from 2015](http://www.atmos.albany.edu/facstaff/brose/classes/ATM623_Spring2015/Notes/index.html).

Many of these notes make use of the `climlab` package, available at https://github.com/brian-rose/climlab

## Contents

1. [Optical properties of individual clouds depend on the Cloud Liquid Water Path](#section1)
2. [Cloudy sky versus clear sky radiation](#section2)
3. [Cloud Radiative Effect (CRE)](#section3)
4. [Modeling the dependence of CRE on cloud height](#section4)
5. [Cloud Feedback](#section5)
6. [Feedback measured in comprehensive GCMs](#section6)
7. [Measuring $\lambda_{clear}$ with radiative kernels](#section7)

____________
<a id='section1'></a>

## 1. Optical properties of individual clouds depend on the Cloud Liquid Water Path
____________


Let $w$ represent the liquid water content of a unit volume of cloudy air, in units of g m$^{-3}$.

Then the Liquid Water Path of the cloud is

$$LWP = w ~ \Delta z$$

where $\Delta z$ is the depth of the cloudy layer in meters.  $LWP$ has units of g m$^{-2}$.

$LWP$ determines the key optical properties of the cloud, both in the longwave and shortwave:


```python
from IPython.display import Image
Image('../images/CloudOpticalProperties_Webster1994.png')
```




![png](output_5_0.png)



A key point about the optical properties of water clouds:

- longwave emissivity / absorptivity increases rapidly with $LWP$
- cloud albedo increases slowly with $LWP$

### Longwave effects of clouds

Because the emissivity saturates for moderately thin clouds, thick clouds behave very much like blackbody absorbers at every level. Emissions from below and within the cloud will be absorbed by the upper part of the cloud.

Emissions to space are therefore **governed by the top of the cloud**.

The longwave effects of a thick cloud thus depend strongly on the **temperature at the top of the cloud**. This temperature is determined primarily by the **height of the cloud top**.

A high-top cloud will exert a strong greenhouse effect because it absorbs upwelling longwave radiation and re-emits radiation at its cold temperature.

The longwave effects of clouds tend to warm the surface.

### Shortwave effects of clouds

Because clouds increase the planetary albedo, the shortwave effects of clouds tend to cool the surface.

The same cloud therefore pushes the planetary energy budget in two directions simultaneously. Which effect dominates depends on

- the temperature at the cloud top relative to the surface temperature
- the cloud liquid water path (cloud depth)

Thin clouds are relatively transparent to solar radiation. Thick clouds are effective reflectors.

A thin cirrus cloud, for example, has a negligible albedo but exerts a substantial greenhouse effect because it is near the cold tropopause. These clouds have a net warming effect.

A relatively thick stratus cloud at the top of the planetary boundary layer reflects significant incoming solar radiation. But the temperature at cloud top is not much different from the surface temperature, so the greenhouse effect is negligible (even though the cloud is a very strong longwave absorber!)

____________
<a id='section2'></a>

## 2. Cloudy sky versus clear sky radiation
____________

Let $F = ASR - OLR$ be the net incoming radiation at TOA.

Suppose that the average flux in the portion of the sky **without clouds** is $F_{clear}$.

We'll call the flux in the cloudy portion of the sky $F_{cloudy}$.

Then the total flux is a weighted sum

$$ F = (1-c) F_{clear} + c F_{cloudy} $$

where $0 \le c \le 1$ is the **cloud fraction**, i.e. the fraction of the sky covered by cloud.

We can of course break this up into long- and shortwave components:

$$ F = F_{LW} + F_{SW} $$

$$ F_{LW} = - \big((1-c)~OLR_{clear} + c ~ OLR_{cloudy} \big)$$

$$ F_{SW} = + \big((1-c)~ASR_{clear} + c ~ ASR_{cloudy} \big)$$

The clouds will act to warm this surface if $F_{cloudy} > F_{clear}$, in which case the net flux $F$ will increase with the cloud fraction $c$.

In our examples above we surmised the following:

#### High thin cirrus
- $ASR_{cloudy} \approx ASR_{clear} $
- $OLR_{cloudy} < OLR_{clear}$
- $F$ increases with $c$ (these clouds warm the surface)

#### Low stratus
- $ASR_{cloudy} < ASR_{clear} $
- $OLR_{cloudy} \approx OLR_{clear}$
- $F$ decreases with $c$ (these clouds cool the surface)

Many other cloud types are ambiguous. For example: 

#### Deep convective cumulonimbus

- $ASR_{cloudy} < ASR_{clear} $
- $OLR_{cloudy} < OLR_{clear}$
- $F$ might either increase or decrease with $c$

We need a model to work out the details!

____________
<a id='section3'></a>

## 3. Cloud Radiative Effect (CRE)
____________

Typically there is not just one cloud type but many to deal with simultaneously, whether in nature (satellite observations) or in a GCM.

In practice we rarely calculate $F_{cloudy}$ explicitly.

Instead we define the **Cloud Radiative Effect** as

$$ CRE =  F - F_{clear} $$

which we can write in terms of cloud fraction:

$$ CRE = c \big( F_{cloudy} - F_{clear} \big) $$

In our above examples, $CRE$ is positive for cirrus, negative for low stratus, and unknown for cumulonimbus.

We calculated CRE (including both longwave and shortwave components) in the CESM simulations back in Assignment 4.

____________
<a id='section4'></a>

## 4. Modeling the dependence of CRE on cloud height
____________

We are now going to use the `RRTMG` radiation model to compute the cloud radiative effect in a single column, and look at how the CRE depends on cloud properties and the height of the cloud layer.


```python
%matplotlib inline
import numpy as np
import matplotlib.pyplot as plt
import climlab
from climlab.radiation import RRTMG
#  Disable interactive plotting (use explicit display calls to show figures)
plt.ioff()
```

### Global average observed temperature and specific humidity


```python
#  Get temperature and humidity data from NCEP Reanalysis
import xarray as xr
from xarray.ufuncs import cos, deg2rad, log, exp
ncep_url = "http://www.esrl.noaa.gov/psd/thredds/dodsC/Datasets/ncep.reanalysis.derived/pressure/"
path = ncep_url
ncep_air = xr.open_dataset(path + 'air.mon.1981-2010.ltm.nc', decode_times=False)
ncep_shum = xr.open_dataset(path + 'shum.mon.1981-2010.ltm.nc', decode_times=False)
```


```python
#  Take global, annual average and convert to correct units (Kelvin and kg/kg)
weight = cos(deg2rad(ncep_air.lat)) / cos(deg2rad(ncep_air.lat)).mean(dim='lat')
Tglobal = (ncep_air.air * weight).mean(dim=('lat','lon','time')) + climlab.constants.tempCtoK
SHglobal = (ncep_shum.shum * weight).mean(dim=('lat','lon','time')) * 1E-3  # kg/kg
```

Since we will be creating a radiative model with a different set of pressure levels than the data, we will need to do some interpolating.


```python
#  Create a state dictionary with 50 levels
state = climlab.column_state(num_lev=50)
lev = state.Tatm.domain.axes['lev'].points
```


```python
# interpolate to model pressure levels
Tinterp = np.interp(lev, np.flipud(Tglobal.level), np.flipud(Tglobal))
SHinterp = np.interp(lev, np.flipud(SHglobal.level), np.flipud(SHglobal))
#  Need to 'flipud' because the interpolation routine 
#  needs the pressure data to be in increasing order
```


```python
#  Plot the temperature and humidity profiles
fig, ax1 = plt.subplots(figsize=(8,5))
Tcolor = 'r'
SHcolor = 'b'
ax1.plot(Tinterp, lev, color=Tcolor)
ax1.invert_yaxis()
ax1.set_xlabel('Temperature (K)', color=Tcolor)
ax1.tick_params('x', colors=Tcolor)
ax1.grid()
ax1.set_ylabel('Pressure (hPa)')
ax2 = ax1.twiny()
ax2.plot(SHinterp*1E3, lev, color=SHcolor)
ax2.set_xlabel('Specific Humidity (g/kg)', color=SHcolor)
ax2.tick_params('x', colors=SHcolor)
fig.suptitle('Global mean air temperature and specific humidity', y=1.03, fontsize=14)
```




    <matplotlib.text.Text at 0x127229ed0>




```python
fig
```




![png](output_32_0.png)




```python
#  Set the temperature to the observed values
state.Tatm[:] = Tinterp
```


```python
#  Define some local cloud characteristics
#  We are going to repeat the calculation 
#   for three different types of clouds:
#   thin, medium, and thick
cldfrac = 0.5  # layer cloud fraction
r_liq = 14.  # Cloud water drop effective radius (microns)
# in-cloud liquid water path (g/m2)
clwp = {'thin': 20.,
        'med': 60.,
        'thick': 200.,}
```


```python
#  Loop through three types of cloud
#  for each type, loop through all pressure levels
#  Set up a radiation model with the cloud layer at the current pressure level
#  Compute CRE and store the results
CRE_LW = {}
CRE_SW = {}
for thickness in clwp:
    OLR = np.zeros_like(lev)
    ASR = np.zeros_like(lev)
    OLRclr = np.zeros_like(lev)
    ASRclr = np.zeros_like(lev)
    for i in range(lev.size):
        # Whole-column cloud characteristics
        #  The cloud fraction is a Gaussian bump centered at the current level        
        mycloud = {'cldfrac': cldfrac*exp(-(lev-lev[i])**2/(2*25.)**2),
                   'clwp': np.zeros_like(state.Tatm) + clwp[thickness],
                   'r_liq': np.zeros_like(state.Tatm) + r_liq,}
        rad = RRTMG(state=state, 
                    albedo=0.2,
                    specific_humidity=SHinterp,
                    verbose=False,
                    **mycloud)
        rad.compute_diagnostics()
        OLR[i] = rad.OLR
        OLRclr[i] = rad.OLRclr
        ASR[i] = rad.ASR
        ASRclr[i] = rad.ASRclr
    CRE_LW[thickness] = -(OLR - OLRclr)
    CRE_SW[thickness] = (ASR - ASRclr)
```


```python
#  Make some plots of the CRE dependence on cloud height
fig, axes = plt.subplots(1,3, figsize=(16,6))
ax = axes[0]
for thickness in clwp:
    ax.plot(CRE_LW[thickness], lev, label=thickness)
ax.set_ylabel('Pressure (hPa)')
ax.set_xlabel('LW cloud radiative effect (W/m2)')

ax = axes[1]
for thickness in clwp:
    ax.plot(CRE_SW[thickness], lev, label=thickness)
ax.set_xlabel('SW cloud radiative effect (W/m2)')

ax = axes[2]
for thickness in clwp:
    ax.plot(CRE_SW[thickness] + CRE_LW[thickness], lev, label=thickness)
ax.set_xlabel('Net cloud radiative effect (W/m2)')

for ax in axes:
    ax.invert_yaxis()
    ax.legend()
    ax.grid()
fig.suptitle('Cloud Radiative Effect as a function of the vertical height of the cloud layer', fontsize=16)
```




    <matplotlib.text.Text at 0x12cc9f710>




```python
fig
```




![png](output_37_0.png)



What do you see here? Look carefully at how the LW and SW effects of the cloud depend on cloud properties and cloud height.

____________
<a id='section5'></a>

## 5. Cloud Feedback
____________

$CRE$ (the radiative effects of clouds) depends on two cloud properties:

- cloud fraction $c$
- cloud LWP, which determines $F_{cloudy}$

If either or both of these things change as the climate changes and the surface warms, then there is an additional TOA energy source that will help determine the final equilibrium warming --  a feedback!

The cloud feedback thus depends on changes in the frequency of occurrence and the optical properties of all the different cloud types. It's an enormously complex problem.

Mathematically: the net climate feedback is

$$ \lambda = \frac{\delta F }{\delta  T_s}  $$

Now using 

$$ F = (1-c) F_{clear} + c F_{cloudy} $$

we can break up the change in $F$ into components due to changes in cloud fraction, clear-sky flux, and cloud optical properties:

$$ \lambda = (1-c)\frac{\delta F_{clear} }{\delta  T_s}  + c \frac{\delta F_{cloudy} }{\delta  T_s} +\big( F_{cloudy} - F_{clear} \big) \frac{\delta c }{\delta  T_s}$$

where $c, F_{cloudy}, F_{clear}$ here would be evaluated from the reference (control) climate, and we assume the changes are small so that the linearization is sensible.

### Clear-sky and cloud feedbacks

It's helpful to gather the second and third terms together in the above expression for $\lambda$ to get

$$ \lambda = (1-c)\frac{\delta F_{clear} }{\delta  T_s}  + c \bigg( \frac{\delta F_{cloudy} }{\delta  T_s} +\big( \frac{F_{cloudy} - F_{clear}}{c} \big) \frac{\delta c }{\delta  T_s} \bigg)$$

Let's now define the **clear-sky feedback**

$$ \lambda_{clear} = \frac{\delta F_{clear} }{\delta  T_s} $$

This includes processes such as Planck feedback, lapse rate feedback, water vapor feedback, and surface albedo feedback.

The second term in our above expression for $\lambda$ involves **changes in cloud fraction and cloud properties**.  We will collectively call these **cloud feedback**, which we now formally define as

$$ \lambda_{cloud} = \frac{\delta F_{cloudy} }{\delta  T_s} +\big( \frac{F_{cloudy} - F_{clear}}{c} \big) \frac{\delta c }{\delta  T_s} $$

So that the net feedback can be written

$$ \lambda = (1-c)~\lambda_{clear} + c~\lambda_{cloud} $$

Remember that all of these expressions can be (and frequently are) decomposed into longwave and shortwave components.

### Cloud feedback vs. CRE

GCM diagnostics usually provide $CRE$ (which are computed by making second passes through the radiation code with the cloud fractions set to zero).

As we did in Assignment 4, we can compute the **change in $CRE$** between a control and perturbation climate.

One key point here is that **the change in $CRE$ is not equivalent to a cloud feedback**.

To see this, let's take the derivative of $CRE = F - F_{clear}$:

$$ \frac{\delta CRE}{\delta T_s} = \frac{\delta F}{\delta T_s} - \frac{\delta F_{clear}}{\delta T_s} $$

Using the above definitions we can write this as

$$ \frac{\delta CRE}{\delta T_s} = \lambda - \lambda_{clear} $$

or

$$ \frac{\delta CRE}{\delta T_s} = c~(\lambda_{cloud} - \lambda_{clear}) $$

The **clear sky feedback** affects the change in $CRE$ we can measure in a GCM, or observations.

Suppose there is **no change in cloud fraction or cloud optical properties**. By definition then $\lambda_{cloud} = 0$. But we would still measure a non-zero change in $CRE$.

Why?

Because the flux in the clear-sky fraction is changing!

#### So how do we compute $\lambda_{cloud}$?

So long as $\lambda_{clear}$ is known, it's easy:

Just measure  $\frac{\delta CRE}{\delta T_s}$ and the cloud fraction $c$ from the model, and solve the above formula to get

$$ \lambda_{cloud}  = \frac{1}{c} \frac{\delta CRE}{\delta T_s} + \lambda_{clear} $$

This is how we can "correct" the change in $CRE$ to get the actual cloud feedback.

____________
<a id='section6'></a>

## 6. Feedback measured in comprehensive GCMs
____________



```python
feedback_ar5 = 'http://www.climatechange2013.org/images/figures/WGI_AR5_Fig9-43.jpg'
Image(url=feedback_ar5)
```




<img src="http://www.climatechange2013.org/images/figures/WGI_AR5_Fig9-43.jpg"/>



> **Figure 9.43** | (a) Strengths of individual feedbacks for CMIP3 and CMIP5 models (left and right columns of symbols) for Planck (P), water vapour (WV), clouds (C), albedo (A), lapse rate (LR), combination of water vapour and lapse rate (WV+LR) and sum of all feedbacks except Planck (ALL), from Soden and Held (2006) and Vial et al. (2013), following Soden et al. (2008). CMIP5 feedbacks are derived from CMIP5 simulations for abrupt fourfold increases in CO2 concentrations (4 × CO2). (b) ECS obtained using regression techniques by Andrews et al. (2012) against ECS estimated from the ratio of CO2 ERF to the sum of all feedbacks. The CO2 ERF is one-half the 4 × CO2 forcings from Andrews et al. (2012), and the total feedback (ALL + Planck) is from Vial et al. (2013).

*Figure caption reproduced from the AR5 WG1 report*

____________
<a id='section7'></a>

## 7. Measuring $\lambda_{clear}$ with radiative kernels
____________



So how are the clear-sky feedbacks (P, WV, LR, A) actually calculated?

Presently, the most popular technique the method of **radiative kernels**.

You have been building a (primitive) kernel for the water vapor feedback in the last homework.


```python
Image('../images/Kernels_Held&Soden2000.png')
```




![png](output_58_0.png)



> Held, I. M. and Soden, B. J. (2000). Water vapor feedback and global warming. Ann. Rev. Energy Environ., 25:441–475.

** These notes are unfinished **

<div class="alert alert-success">
[Back to ATM 623 notebook home](../index.ipynb)
</div>

____________
## Version information
____________




```python
%load_ext version_information
%version_information numpy, matplotlib, climlab
```




<table><tr><th>Software</th><th>Version</th></tr><tr><td>Python</td><td>2.7.12 64bit [GCC 4.2.1 (Based on Apple Inc. build 5658) (LLVM build 2336.11.00)]</td></tr><tr><td>IPython</td><td>5.3.0</td></tr><tr><td>OS</td><td>Darwin 16.5.0 x86_64 i386 64bit</td></tr><tr><td>numpy</td><td>1.11.1</td></tr><tr><td>matplotlib</td><td>2.0.0</td></tr><tr><td>climlab</td><td>0.5.6</td></tr><tr><td colspan='2'>Thu May 25 13:27:26 2017 EDT</td></tr></table>



____________

## Credits

The author of this notebook is [Brian E. J. Rose](http://www.atmos.albany.edu/facstaff/brose/index.html), University at Albany.

It was developed in support of [ATM 623: Climate Modeling](http://www.atmos.albany.edu/facstaff/brose/classes/ATM623_Spring2015/), a graduate-level course in the [Department of Atmospheric and Envionmental Sciences](http://www.albany.edu/atmos/index.php)

Development of these notes and the [climlab software](https://github.com/brian-rose/climlab) is partially supported by the National Science Foundation under award AGS-1455071 to Brian Rose. Any opinions, findings, conclusions or recommendations expressed here are mine and do not necessarily reflect the views of the National Science Foundation.
____________


```python

```
