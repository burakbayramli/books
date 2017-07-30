
# [ATM 623: Climate Modeling](../index.ipynb)

[Brian E. J. Rose](http://www.atmos.albany.edu/facstaff/brose/index.html), University at Albany

# Lecture 19: Snowball Earth and Large Ice Cap Instability in the EBM

### About these notes:

This document uses the interactive [`Jupyter notebook`](https://jupyter.org) format. The notes can be accessed in several different ways:

- The interactive notebooks are hosted on `github` at https://github.com/brian-rose/ClimateModeling_courseware
- The latest versions can be viewed as static web pages [rendered on nbviewer](http://nbviewer.ipython.org/github/brian-rose/ClimateModeling_courseware/blob/master/index.ipynb)
- A complete snapshot of the notes as of May 2017 (end of spring semester) are [available on Brian's website](http://www.atmos.albany.edu/facstaff/brose/classes/ATM623_Spring2017/Notes/index.html).

[Also here is a legacy version from 2015](http://www.atmos.albany.edu/facstaff/brose/classes/ATM623_Spring2015/Notes/index.html).

Many of these notes make use of the `climlab` package, available at https://github.com/brian-rose/climlab

<div class="alert alert-warning">
Unlike most of the other notebooks in this series, this one performs some intensive computations and may take more than an hour to run from start to finish (your mileage may vary).
</div>

## Contents

1. [The Neoproterozoic Snowball Earth events](#section1)
2. [Stability of the polar ice caps: the concept](#section2)
3. [Annual-mean EBM with albedo feedback: adjustment to equilibrium](#section3)
4. [Global feedback analysis in the EBM](#section4)
5. [Here Comes the Sun! Where is the ice edge?](#section5)



```python
%matplotlib inline
import numpy as np
import matplotlib.pyplot as plt
from climlab import constants as const
import climlab
#  Disable interactive plotting (use explicit display calls to show figures)
plt.ioff()
```


```python
from IPython.display import Image
```

____________
<a id='section1'></a>

## 1. The Neoproterozoic Snowball Earth events
____________



### The Geologic Time Scale

First, some information on the nomenclature for Earth history:


```python
Image('../images/GeoTimeScale2009.png')
```




![png](output_8_0.png)



> Walker, J. and Geissman, J. (2009). Geologic time scale. Technical report, Geological Society of America.

### Extensive evidence for large glaciers at sea level in the tropics 

Evidently the climate was **very cold** at these times (635 Ma and 715 Ma)


```python
Image('../images/Hoffman_Li_2009.png')
```




![png](output_11_0.png)



> Hoffman, P. F. and Li, Z.-X. (2009). A palaeogeographic context for Neoproterozoic glaciation. Palaeogeogr. Palaeoclimatol. Palaeoecol., 277:158–172.


```python
Image('../images/Hoffman_Schrag_2002_Fig2.png')
```




![png](output_13_0.png)



> Hoffman, P. F. and Schrag, D. P. (2002). The snowball Earth hypothesis: testing the limits of global change. Terra Nova, 14(3):129–155.

###  The Snowball Earth hypothesis

Various bizarre features in the geological record from 635 and 715 Ma ago indicate that the Earth underwent some very extreme environmental changes… at least twice. The **Snowball Earth hypothesis** postulates that:

- The Earth was completely ice-covered (including the oceans)
- The total glaciation endured for millions of years
- CO$_2$ slowly accumulated in the atmosphere from volcanoes
- Weathering of rocks (normally acting to reduce CO$_2$) extremely slow due to cold, dry climate
- Eventually the extreme greenhouse effect is enough to melt back the ice
- The Earth then enters a period of extremely hot climate.

The hypothesis rests on a phenomenon first discovered by climate modelers in the Budyko-Sellers EBM: **runaway ice-albedo feedback** or **large ice cap instability**.

____________
<a id='section2'></a>

## 2. Stability of the polar ice caps: the concept
____________



```python
Image('../images/large_ice_cap_sketch.png')
```




![png](output_18_0.png)



For small perturbations, we can relate the cooling to the displacement of the ice edge as:

$$  \delta T = -\frac{dT}{d\phi} \bigg|_{\phi_i} \delta \phi  $$

### Stability criterion:

$$ \frac{\delta a ~ S(\phi_i) ~ \cos\phi_i}{B} < -\frac{dT}{d\phi} \bigg|_{\phi_i} \delta \phi  $$

The LHS is **largest at the equator**, while the RHS is **large in mid-latitudes and near zero at the equator**.

The conclusion:  **The ice will always grow unstably toward the equator once it reaches some critical latitude.**

This is called **Large Ice Cap Instability**, and was first discovered in the Budyko-Sellers EBM.

See [Rose (2015), J. Geophys. Res., doi:10.1002/2014JD022659](http://onlinelibrary.wiley.com/doi/10.1002/2014JD022659/abstract)

____________
<a id='section3'></a>

## 3. Annual-mean EBM with albedo feedback: adjustment to equilibrium
____________

Here we will use the 1-dimensional diffusive Energy Balance Model (EBM) to explore the effects of albedo feedback and heat transport on climate sensitivity.


```python
#  for convenience, set up a dictionary with our reference parameters
param = {'A':210, 'B':2, 'a0':0.3, 'a2':0.078, 'ai':0.62, 'Tf':-10., 'D':0.55}
model1 = climlab.EBM_annual( num_lat=180, **param )
print model1
```

    climlab Process of type <class 'climlab.model.ebm.EBM_annual'>. 
    State variables and domain shapes: 
      Ts: (180, 1) 
    The subprocess tree: 
    top: <class 'climlab.model.ebm.EBM_annual'>
       diffusion: <class 'climlab.dynamics.diffusion.MeridionalDiffusion'>
       LW: <class 'climlab.radiation.aplusbt.AplusBT'>
       albedo: <class 'climlab.surface.albedo.StepFunctionAlbedo'>
          iceline: <class 'climlab.surface.albedo.Iceline'>
          cold_albedo: <class 'climlab.surface.albedo.ConstantAlbedo'>
          warm_albedo: <class 'climlab.surface.albedo.P2Albedo'>
       insolation: <class 'climlab.radiation.insolation.AnnualMeanInsolation'>
    



```python
model1.integrate_years(5)
Tequil = np.array(model1.Ts)
ALBequil = np.array(model1.albedo)
OLRequil = np.array(model1.OLR)
ASRequil = np.array(model1.ASR)
```

    Integrating for 450 steps, 1826.211 days, or 5 years.
    Total elapsed time is 5.0 years.


Let's look at what happens if we perturb the temperature -- make it 20ºC colder everywhere!


```python
model1.Ts -= 20.
model1.compute_diagnostics()
```

Let's take a look at how we have just perturbed the absorbed shortwave:


```python
my_ticks = [-90,-60,-30,0,30,60,90]
lat = model1.lat

fig = plt.figure( figsize=(12,5) )

ax1 = fig.add_subplot(1,2,1)
ax1.plot(lat, Tequil, label='equil') 
ax1.plot(lat, model1.state['Ts'], label='pert' )
ax1.grid()
ax1.legend()
ax1.set_xlim(-90,90)
ax1.set_xticks(my_ticks)
ax1.set_xlabel('Latitude')
ax1.set_ylabel('Temperature (degC)')

ax2 = fig.add_subplot(1,2,2)
ax2.plot( lat, ASRequil, label='equil') 
ax2.plot( lat, model1.diagnostics['ASR'], label='pert' )
ax2.grid()
ax2.legend()
ax2.set_xlim(-90,90)
ax2.set_xticks(my_ticks)
ax2.set_xlabel('Latitude')
ax2.set_ylabel('ASR (W m$^{-2}$)')
```




    <matplotlib.text.Text at 0x1175b4ad0>




```python
fig
```




![png](output_29_0.png)



So there is less absorbed shortwave now, because of the increased albedo. The global mean difference is:


```python
climlab.global_mean( model1.ASR - ASRequil )
```




    -20.374400181491197



Less shortwave means that there is a tendency for the climate to cool down even more! In other words, the shortwave feedback is **positive**.

____________
<a id='section4'></a>

## 4. Global feedback analysis in the EBM
____________

Take the global mean of the EBM equation, the transport term drops out

$$ C \frac{\partial \langle T_s \rangle}{\partial t} = \langle (1-\alpha) ~ Q \rangle - ~A - B~\langle T_s \rangle $$


Way back in [Lecture 3](Lecture03 -- Climate sensitivity and feedback.ipynb) we defined the net climate feedback through a linear Taylor series expansion of the global energy budget:

$$ \lambda = \frac{\partial}{\partial \langle T_s \rangle} \bigg( \Delta F_{TOA} \bigg) $$

where $\Delta F_{TOA}$ is the change in the net downward radiative flux at the TOA associated with a change $\delta \langle T_s \rangle$ in the global average surface temperature.

Applying this to the RHS of our EBM equation gives

$$ \lambda = \lambda_{LW} + \lambda_{SW} $$

with longwave and shortwave contributions:

$$ \lambda_{LW} = -B $$

and 

$$ \lambda_{SW} = \frac{\Delta \langle (1-\alpha) ~ Q \rangle}{\Delta \langle T_s \rangle} $$

The longwave feedback is a constant in our model, by contruction.

The shortwave feedback, on the other hand, is **state-dependent**. The feedback depends on the detailed displacement of the ice line for a given global temperature change.

Plugging in numbers from our example above gives

$\lambda = - 2 + \frac{-20.4}{-20} = -2 + 1 = -1$ W m$^{-2}$ $^{\circ}$C$^{-1}$

The feedback is negative, as we expect! The tendency to warm up from reduced OLR outweighs the tendency to cool down from reduced ASR. A negative net feedback means that the system will relax back towards the equilibrium.

Let's let the temperature evolve one year at a time and add extra lines to the graph:


```python
plt.plot( lat, Tequil, 'k--', label='equil' )
plt.plot( lat, model1.Ts, 'k-', label='pert' )
plt.grid(); plt.xlim(-90,90); plt.legend()
for n in range(5):
    model1.integrate_years(years=1.0, verbose=False)
    plt.plot(lat, model1.Ts)
plt.show()
```


![png](output_39_0.png)


Temperature drifts back towards equilibrium, as we expected!

What if we cool the climate **so much** that the entire planet is ice covered?


```python
model1.Ts -= 40.
model1.compute_diagnostics()
```

Look again at the change in absorbed shortwave:


```python
climlab.global_mean( model1.ASR - ASRequil )
```




    -108.99364172844652



It's much larger because we've covered so much more surface area with ice!

The feedback calculation now looks like

$\lambda = - 2 + \frac{-109}{-40} = -2 + 2.7 = +0.7$ W m$^{-2}$ $^{\circ}$C$^{-1}$

What? Looks like the **positive** albedo feedback is so strong here that it has outweighed the **negative** longwave feedback. What will happen to the system now? Let's find out...


```python
plt.plot( lat, Tequil, 'k--', label='equil' )
plt.plot( lat, model1.Ts, 'k-', label='pert' )
plt.grid(); plt.xlim(-90,90); plt.legend()
for n in range(5):
    model1.integrate_years(years=1.0, verbose=False)
    plt.plot(lat, model1.Ts)
plt.show()
```


![png](output_46_0.png)


Something **very different** happened! The climate drifted towards an entirely different equilibrium state, in which the entire planet is cold and ice-covered.

We will refer to this as the **SNOWBALL EARTH**.

Note that the warmest spot on the planet is still the equator, but it is now about -33ºC rather than +28ºC!

____________
<a id='section5'></a>

## 5. Here Comes the Sun! Where is the ice edge?
____________

The ice edge in our model is always where the temperature crosses $T_f = -10^\circ$C. The system is at **equilibrium** when the temperature is such that there is a balance between ASR, OLR, and heat transport convergence everywhere. 

Suppose that sun was hotter or cooler at different times (in fact it was significantly cooler during early Earth history). That would mean that the solar constant $S_0 = 4Q$ was larger or smaller. We should expect that the temperature (and thus the ice edge) should increase and decrease as we change $S_0$. 

$S_0$ during the Neoproterozoic Snowball Earth events is believed to be about 93% of its present-day value, or about 1270 W m$^{-2}$.

We are going to look at how the **equilibrium** ice edge depends on $S_0$, by integrating the model out to equilibrium for lots of different values of $S_0$. We will start by slowly decreasing $S_0$, and then slowly increasing $S_0$.


```python
model2 = climlab.EBM_annual(num_lat = 360, **param)
```


```python
S0array = np.linspace(1400., 1200., 200.)
```


```python
model2.integrate_years(5)
```

    Integrating for 450 steps, 1826.211 days, or 5 years.
    Total elapsed time is 5.0 years.



```python
print model2.icelat
```

    [-71.  71.]



```python
icelat_cooling = np.empty_like(S0array)
icelat_warming = np.empty_like(S0array)
```


```python
# First cool....
for n in range(S0array.size):
    model2.subprocess['insolation'].S0 = S0array[n]
    model2.integrate_years(10, verbose=False)
    icelat_cooling[n] = np.max(model2.icelat)
# Then warm...
for n in range(S0array.size):
    model2.subprocess['insolation'].S0 = np.flipud(S0array)[n]
    model2.integrate_years(10, verbose=False)
    icelat_warming[n] = np.max(model2.icelat)
```

For completeness: also start from present-day conditions and warm up.


```python
model3 = climlab.EBM_annual(num_lat=360, **param)
S0array3 = np.linspace(1350., 1400., 50.)
#S0array3 = np.linspace(1350., 1400., 5.)
icelat3 = np.empty_like(S0array3)
```


```python
for n in range(S0array3.size):
    model3.subprocess['insolation'].S0 = S0array3[n]
    model3.integrate_years(10, verbose=False)
    icelat3[n] = np.max(model3.icelat)
```


```python
fig = plt.figure( figsize=(10,6) )
ax = fig.add_subplot(111)
ax.plot(S0array, icelat_cooling, 'r-', label='cooling' )
ax.plot(S0array, icelat_warming, 'b-', label='warming' )
ax.plot(S0array3, icelat3, 'g-', label='warming' )
ax.set_ylim(-10,100)
ax.set_yticks((0,15,30,45,60,75,90))
ax.grid()
ax.set_ylabel('Ice edge latitude', fontsize=16)
ax.set_xlabel('Solar constant (W m$^{-2}$)', fontsize=16)
ax.plot( [const.S0, const.S0], [-10, 100], 'k--', label='present-day' )
ax.legend(loc='upper left')
ax.set_title('Solar constant versus ice edge latitude in the EBM with albedo feedback', fontsize=16);
```


```python
fig
```




![png](output_61_0.png)



There are actually up to 3 different climates possible for a given value of $S_0$!

### How to un-freeze the Snowball

The graph indicates that if the Earth were completely frozen over, it would be perfectly happy to stay that way even if the sun were brighter and hotter than it is today.

Our EBM predicts that (with present-day parameters) the equilibrium temperature at the equator in the Snowball state is about -33ºC, which is much colder than the threshold temperature $T_f = -10^\circ$C. How can we melt the Snowball?

We need to increase the avaible energy sufficiently to get the equatorial temperatures above this threshold! That is going to require a much larger increase in $S_0$ (could also increase the greenhouse gases, which would have a similar effect)!

Let's crank up the sun to 1830 W m$^{-2}$ (about a 35% increase from present-day).


```python
model4 = climlab.process_like(model2)  # initialize with cold Snowball temperature
model4.subprocess['insolation'].S0 = 1830.
model4.integrate_years(40)
plt.plot(model4.lat, model4.Ts)
plt.xlim(-90,90); plt.ylabel('Temperature'); plt.xlabel('Latitude')
plt.grid(); plt.xticks(my_ticks)
print('The ice edge is at ' + str(model4.icelat) + ' degrees latitude.' )
plt.show()
```

    Integrating for 3600 steps, 14609.688 days, or 40 years.
    Total elapsed time is 4044.99999998 years.
    The ice edge is at [-0.  0.] degrees latitude.



![png](output_66_1.png)


Still a Snowball... but just barely! The temperature at the equator is just below the threshold.

Try to imagine what might happen once it starts to melt. The solar constant is huge, and if it weren't for the highly reflective ice and snow, the climate would be really really hot!

We're going to increase $S_0$ one more time...


```python
model4.subprocess['insolation'].S0 = 1840.
model4.integrate_years(10)
plt.plot(model4.lat, model4.Ts)
plt.xlim(-90,90); plt.ylabel('Temperature'); plt.xlabel('Latitude')
plt.grid(); plt.xticks(my_ticks);
plt.show()
```

    Integrating for 900 steps, 3652.422 days, or 10 years.
    Total elapsed time is 4054.99999998 years.



![png](output_68_1.png)


Suddenly the climate looks very very different again! The global mean temperature is


```python
print( model4.global_mean_temperature() )
```

    57.7335544708


A roasty 58ºC, and the poles are above 20ºC. A tiny increase in $S_0$ has led to a very drastic change in the climate.


```python
S0array_snowballmelt = np.linspace(1400., 1900., 50)
icelat_snowballmelt = np.empty_like(S0array_snowballmelt)
icelat_snowballmelt_cooling = np.empty_like(S0array_snowballmelt)

for n in range(S0array_snowballmelt.size):
    model2.subprocess['insolation'].S0 = S0array_snowballmelt[n]
    model2.integrate_years(10, verbose=False)
    icelat_snowballmelt[n] = np.max(model2.icelat)
    
for n in range(S0array_snowballmelt.size):
    model2.subprocess['insolation'].S0 = np.flipud(S0array_snowballmelt)[n]
    model2.integrate_years(10, verbose=False)
    icelat_snowballmelt_cooling[n] = np.max(model2.icelat)
```

Now we will complete the plot of ice edge versus solar constant.


```python
fig = plt.figure( figsize=(10,6) )
ax = fig.add_subplot(111)
ax.plot(S0array, icelat_cooling, 'r-', label='cooling' )
ax.plot(S0array, icelat_warming, 'b-', label='warming' )
ax.plot(S0array3, icelat3, 'g-', label='warming' )
ax.plot(S0array_snowballmelt, icelat_snowballmelt, 'b-' )
ax.plot(S0array_snowballmelt, icelat_snowballmelt_cooling, 'r-' )
ax.set_ylim(-10,100)
ax.set_yticks((0,15,30,45,60,75,90))
ax.grid()
ax.set_ylabel('Ice edge latitude', fontsize=16)
ax.set_xlabel('Solar constant (W m$^{-2}$)', fontsize=16)
ax.plot( [const.S0, const.S0], [-10, 100], 'k--', label='present-day' )
ax.legend(loc='upper left')
ax.set_title('Solar constant versus ice edge latitude in the EBM with albedo feedback', fontsize=16);
```


```python
fig
```




![png](output_75_0.png)



The upshot:

- For extremely large $S_0$, the only possible climate is a hot Earth with no ice.
- For extremely small $S_0$, the only possible climate is a cold Earth completely covered in ice.
- For a large range of $S_0$ including the present-day value, more than one climate is possible!
- Once we get into a Snowball Earth state, getting out again is rather difficult!

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




<table><tr><th>Software</th><th>Version</th></tr><tr><td>Python</td><td>2.7.12 64bit [GCC 4.2.1 (Based on Apple Inc. build 5658) (LLVM build 2336.11.00)]</td></tr><tr><td>IPython</td><td>5.3.0</td></tr><tr><td>OS</td><td>Darwin 16.5.0 x86_64 i386 64bit</td></tr><tr><td>numpy</td><td>1.11.1</td></tr><tr><td>matplotlib</td><td>2.0.0</td></tr><tr><td>climlab</td><td>0.5.6</td></tr><tr><td colspan='2'>Thu May 25 11:35:48 2017 EDT</td></tr></table>



____________

## Credits

The author of this notebook is [Brian E. J. Rose](http://www.atmos.albany.edu/facstaff/brose/index.html), University at Albany.

It was developed in support of [ATM 623: Climate Modeling](http://www.atmos.albany.edu/facstaff/brose/classes/ATM623_Spring2015/), a graduate-level course in the [Department of Atmospheric and Envionmental Sciences](http://www.albany.edu/atmos/index.php)

Development of these notes and the [climlab software](https://github.com/brian-rose/climlab) is partially supported by the National Science Foundation under award AGS-1455071 to Brian Rose. Any opinions, findings, conclusions or recommendations expressed here are mine and do not necessarily reflect the views of the National Science Foundation.
____________


```python

```
