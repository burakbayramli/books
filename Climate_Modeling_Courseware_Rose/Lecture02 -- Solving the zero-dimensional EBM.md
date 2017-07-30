
# [ATM 623: Climate Modeling](../index.ipynb)

[Brian E. J. Rose](http://www.atmos.albany.edu/facstaff/brose/index.html), University at Albany

# Lecture 2: Solving the zero-dimensional energy balance model

### About these notes:

This document uses the interactive [`Jupyter notebook`](https://jupyter.org) format. The notes can be accessed in several different ways:

- The interactive notebooks are hosted on `github` at https://github.com/brian-rose/ClimateModeling_courseware
- The latest versions can be viewed as static web pages [rendered on nbviewer](http://nbviewer.ipython.org/github/brian-rose/ClimateModeling_courseware/blob/master/index.ipynb)
- A complete snapshot of the notes as of May 2017 (end of spring semester) are [available on Brian's website](http://www.atmos.albany.edu/facstaff/brose/classes/ATM623_Spring2017/Notes/index.html).

[Also here is a legacy version from 2015](http://www.atmos.albany.edu/facstaff/brose/classes/ATM623_Spring2015/Notes/index.html).

Many of these notes make use of the `climlab` package, available at https://github.com/brian-rose/climlab

### From last class:

- we wrote down a budget for the energy content of the global climate system
- we wrote the OLR in terms of an effective emission temperature $T_e$
- The equilibrium emission temperature for Earth is about $T_e \approx 255$ K
- This depends only on energy output from the Sun, and the planetary albedo
- We assumed that global energy content is proportional to surface temperature $T_s$
- We thus have a single equation in two unknown temperatures:

$$C \frac{d T_s}{dt} = (1-\alpha) Q - \sigma T_e^4$$


### Parameterizing the dependence of OLR on surface temperature

Later, we will introduce additional physics for column radiative transfer to link $T_s$ and $T_e$.
For now, we'll make the **simplest assumption** we can:

$$ T_e = \beta T_s$$

where $\beta$ is a dimensionless constant. This is a **parameterization** that we introduce into the model for simplicity. We need a value for our **parameter** $\beta$, which we will get from observations:


```python
sigma = 5.67E-8  #  Stefan-Boltzmann constant in W/m2/K4
Q = 341.3  #  global mean insolation in W/m2
alpha = 101.9 / Q    #  observed planetary albedo
print alpha
Te = ((1-alpha)*Q/sigma)**0.25  #  Emission temperature (definition)
print Te
Tsbar = 288.  # global mean surface temperature in K
beta = Te / Tsbar   #  Calculate value of beta from observations
print beta
```

    0.298564312921
    254.909060187
    0.885100903427


Using this parameterization, we can now write a closed equation for surface temperature:

$$C \frac{d T_s}{dt}=(1-α)Q-σ(\beta T_s )^4$$

### Solving the energy balance model

This is a first-order Ordinary Differential Equation (ODE) for $T_s$ as a function of time. It is also our very first climate model.

To solve it (i.e. see how $T_s$ evolves from some specified initial condition) we have two choices:

1. Solve it analytically
2. Solve it numerically

Option 1 (analytical) will usually not be possible because the equations will typically be too complex and non-linear. This is why computers are our best friends in the world of climate modeling.

HOWEVER it is often useful and instructive to simplify a model down to something that is analytically solvable when possible. Why? Two reasons:

1. Analysis will often yield a deeper understanding of the behavior of the system
2. Gives us a benchmark against which to test the results of our numerical solutions.

### Equilibrium solutions

Note that equilibrium surface temperature has been tuned to observations: 

$$\bar{T_s} = \frac{1}{β} \bigg( \frac{(1-α)Q}{σ}\bigg)^{\frac{1}{4}} = 288 K $$


```python
print ((1-alpha)*Q/sigma)**0.25 /beta
```

    288.0


We are going to linearize the equation for small perturbations away from this equilibrium.

Let $T_s = \bar{T_s} + T_s^\prime$ and restrict our solution to $T_s^\prime << \bar{T_s}$.
Note this this is not a big restriction! For example, a 10 degree warming or cooling is just $\pm$3.4% of the absolute equilibrium temperature.

### Linearizing the governing equation

Now use a first-order Taylor series expansion to write

$$OLR \approx \sigma \big(\beta T_s \big)^4 \approx \sigma \big(\beta \bar{T_s} \big)^4 + \Big(4 \sigma \beta^4 \bar{T_s}^3 \Big) T_s^\prime $$

and the budget for the perturbation temperature thus becomes

$$C \frac{d T_s^\prime}{d t} = -\lambda_0 T_s^\prime$$

where we define

$$\lambda_0 = 4 \sigma \beta^4 \bar{T_s}^3$$

Putting in our observational values, we get 


```python
lambda_0 = 4 * sigma * beta**4 * Tsbar**3
#  This is an example of formatted text output in Python
print 'lambda_0 = {:.2f} W m-2 K-1'.format(lambda_0)  
```

    lambda_0 = 3.33 W m-2 K-1


This is actually our first estimate of what is often called the **Planck feedback**. It is the tendency for a warm surface to cool by increased longwave radiation to space. 

It may also be refered to as the "no-feedback" climate response parameter. As we will see, $\lambda_0$ quantifies the sensitivity of the climate system in the absence of any actual feedback processes.

### Solve the linear ODE

Now define

$$ \tau = \frac{C}{\lambda_0}  $$

This is a positive constant with dimensions of time (seconds). With these definitions the temperature evolves according to

$$ \frac{d T_s^\prime}{d t} = - \frac{T_s^\prime}{\tau}$$

This is one of the simplest ODEs. Hopefully it looks familiar to most of you. It is the equation for an **exponential decay** process. 

We can easily solve for the temperature evolution by integrating from an initial condition $T_s^\prime(0)$:

$$ \int_{T_s^\prime(0)}^{T_s^\prime(t)} \frac{d T_s^\prime}{T_s^\prime} = -\int_0^t  \frac{dt}{\tau}$$

$$\ln \bigg( \frac{T_s^\prime(t)}{T_s^\prime(0)} \bigg) = -\frac{t}{\tau}$$

$$T_s^\prime(t) = T_s^\prime(0) \exp \bigg(-\frac{t}{\tau} \bigg)$$

I hope that the mathematics is straightforward for everyone in this class. If not, go through it carefully and make sure you understand each step.

### e-folding time for relaxation of global mean temperature

Our model says that surface temperature will relax toward its equilibrium value over a characteristic time scale $\tau$. This is an **e-folding time** – the time it takes for the perturbation to decay by a factor $1/e = 0.37$

*What should this timescale be for the climate system?*

To estimate $\tau$ we need a value for the effective heat capacity $C$.
A quick and dirty estimate:

$$C = c_w \rho_w H$$

where 

$c_w = 4 \times 10^3$ J kg$^{-1}$ $^\circ$C$^{-1}$ is the specific heat of water,

$\rho_w = 10^3$ kg m$^{-3}$ is the density of water, and

$H$ is an effective depth of water that is heated or cooled.

#### What is the right choice for water depth $H$? 

That turns out to be an interesting and subtle question. It depends very much on the timescale of the problem

- days?
- years?
- decades?
- millenia?

We will revisit this question later in the course. For now, let’s just assume that $H = 100$ m (a bit deeper than the typical depth of the surface mixed layer in the oceans).

Now calculate the e-folding time for the surface temperature:


```python
c_w = 4E3  #  Specific heat of water in J/kg/K
rho_w = 1E3  #  Density of water in kg/m3
H = 100.   #  Depth of water in m
C = c_w * rho_w * H   #  Heat capacity of the model in J/m2/K
tau = C / lambda_0   #  Calculated value of relaxation time constant
seconds_per_year = 60.*60.*24.*365.
print 'The e-folding time is {:1.2e} seconds or about {:1.0f} years.'.format(tau, tau / seconds_per_year)
```

    The e-folding time is 1.20e+08 seconds or about 4 years.


This is a rather fast timescale relative to other processes that can affect the planetary energy budget. 

**But notice that the climate feedback parameter $\lambda$ is smaller, the timescale gets longer.**  We will come back to this later.

### Some take-away messages:

- Earth (or any planet) has a well-defined equilibrium temperature because of the temperature dependence of the outgoing longwave radiation.
- The system will tend to relax toward its equilibrium temperature on an $e$-folding timescale that depends on (1) radiative feedback processes, and (2) effective heat capacity.
- In our estimate, this e-folding time is relatively short. In the absence of other processes that can either increase the heat capacity or lower (in absolute value) the feedback parameter, the Earth would never be very far out of energy balance
- We will quantify this statement more as the term progresses.

### Plotting the solution in Python

Here I'm going to show some example code for making simple line plots with Python. I strongly encourage you to try this out on your own. 

**Avoid the temptation to copy and paste the code!** You won't learn anything that way. Type the code into your own Python session. Experiment with it!


```python
# This code uses the `numpy` package to do efficient array operations. 
# Before we use the package, we import it into the current Python session.
import numpy as np
t = np.linspace(0, 5*tau)  # a time array
print t
```

    [  0.00000000e+00   1.22755869e+07   2.45511739e+07   3.68267608e+07
       4.91023477e+07   6.13779346e+07   7.36535216e+07   8.59291085e+07
       9.82046954e+07   1.10480282e+08   1.22755869e+08   1.35031456e+08
       1.47307043e+08   1.59582630e+08   1.71858217e+08   1.84133804e+08
       1.96409391e+08   2.08684978e+08   2.20960565e+08   2.33236152e+08
       2.45511739e+08   2.57787325e+08   2.70062912e+08   2.82338499e+08
       2.94614086e+08   3.06889673e+08   3.19165260e+08   3.31440847e+08
       3.43716434e+08   3.55992021e+08   3.68267608e+08   3.80543195e+08
       3.92818782e+08   4.05094369e+08   4.17369956e+08   4.29645542e+08
       4.41921129e+08   4.54196716e+08   4.66472303e+08   4.78747890e+08
       4.91023477e+08   5.03299064e+08   5.15574651e+08   5.27850238e+08
       5.40125825e+08   5.52401412e+08   5.64676999e+08   5.76952586e+08
       5.89228172e+08   6.01503759e+08]



```python
type(t)  # this shows that t is numpy.ndarray type
```




    numpy.ndarray




```python
t.shape  # a tuple showing the dimensions of the array
```




    (50,)




```python
Tsprime0 = 6.  # initial temperature perturbation
#  Here we define the actual solution
Tsprime = Tsbar + Tsprime0 * np.exp(-t/tau)  
Tsprime.shape
#  got the same size array
#  the numpy function np.exp() operated simultaneously
#  on all elements of the array
```




    (50,)




```python
#  And here is the solution array
print Tsprime
```

    [ 294.          293.41795616  292.89237483  292.41777873  291.98922192
      291.60223825  291.25279482  290.93724996  290.65231525  290.3950213
      290.16268673  289.95289032  289.76344569  289.59237857  289.43790622
      289.29841881  289.1724627   289.05872525  288.95602117  288.86328013
      288.77953565  288.703915    288.6356301   288.57396934  288.51829012
      288.46801219  288.42261159  288.38161518  288.34459572  288.31116741
      288.2809819   288.2537246   288.22911146  288.20688598  288.18681653
      288.16869396  288.15232941  288.13755235  288.12420876  288.11215961
      288.1012793   288.09145447  288.08258272  288.07457159  288.0673376
      288.06080536  288.0549068   288.04958044  288.04477077  288.04042768]


To make a plot, we will use the `matplotlib` library. The plotting commands work a lot like `MATLAB`. But, like any other package, we need to import it before using it.


```python
# pyplot is the name of the library of plotting routines within matplotlib
#  here we import them and give them a "nickname"
import matplotlib.pyplot as plt
```


```python
#  this command allows the plots to appear inline in this notebook
%matplotlib inline
```


```python
plt.plot(t, Tsprime)
```




    [<matplotlib.lines.Line2D at 0x10f181410>]




![png](output_32_1.png)



```python
#  use a more convenient unit for time
plt.plot(t / seconds_per_year, Tsprime) 
```




    [<matplotlib.lines.Line2D at 0x10f763210>]




![png](output_33_1.png)



```python
#  Or add some helpful labels
plt.plot(t / seconds_per_year, Tsprime) 
plt.xlabel('Years')
plt.ylabel('Global mean temperature (K)')
plt.title('Relaxation to equilibrium temperature')
```




    <matplotlib.text.Text at 0x10f97c650>




![png](output_34_1.png)


### Solving the ODE numerically

In this case the equation is sufficiently simple that we have an analytical solution. Most models are too mathematically complex for this, and we need numerical solution methods. Because the governing equations for every model are differential in time (and usually in space as well), we need to use some numerical approximations to the governing equations.

We approximate the time derivative with

$$ \frac{dT}{dt} \approx \frac{∆T}{∆t} = \frac{T_1-T_0}{t_1-t_0} $$

which makes sense as long as the **timestep** $\delta t$ is *sufficiently small*.

What is meant by sufficiently small? In practice, small enough that the numerical solution behaves well! We will not spend much time in this course talking about numerical methods, but there is much we could say about this…

### Explicit time discretization

The simplest time discretization is called **Forward Euler** or **Explicit Euler**. Say we know the state of the system at time $t_0$, i.e. we know the temperature $T_0$. Then rearranging the above,

$$T_1 = T_0 + ∆t (dT/dt)$$

So if we can calculate the **tendency** of the system (i.e. the time derivative) at time $t_0$, then we have a formula to predict the next state of the system.

For our linearized zero-dimensional energy balance model, 

$$\frac{dT_s}{dt} = -\frac{1}{\tau} \big(T_s-\bar{T_s} \big)$$

So we can predict the temperature with

$$ T_1 = T_0 -  \frac{\Delta t}{\tau} \big( T_0 - \bar{T_s} \big)$$

Let’s implement this formula as a simple function in Python to calculate the next temperature at each timestep


```python
def next_temperature(T0, timestep, tau):
    Tsbar = 288.
    return T0 - timestep/tau * (T0-Tsbar)
```

Now let’s construct the full numerical solution by looping in time:


```python
Tnumerical = np.zeros_like(t)
print Tnumerical
print Tnumerical.size
```

    [ 0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.
      0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.
      0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.]
    50



```python
# Assign the initial condition
Tnumerical[0] = Tsprime0 + Tsbar
print Tnumerical
# this shows indexing of the time array. t[0] is the first element
# t[1] is the second element
#  in Python we always start counting from zero
```

    [ 294.    0.    0.    0.    0.    0.    0.    0.    0.    0.    0.    0.
        0.    0.    0.    0.    0.    0.    0.    0.    0.    0.    0.    0.
        0.    0.    0.    0.    0.    0.    0.    0.    0.    0.    0.    0.
        0.    0.    0.    0.    0.    0.    0.    0.    0.    0.    0.    0.
        0.    0.]



```python
timestep = t[1] - t[0] 
for i in range(Tnumerical.size-1):
    # assign the next temperature value to the approprate array element
    Tnumerical[i+1] = next_temperature(Tnumerical[i], timestep, tau)
print Tnumerical
```

    [ 294.          293.3877551   292.83798417  292.34431232  291.90101514
      291.50295237  291.14550825  290.82453802  290.53631986  290.27751171
      290.04511256  289.8364276   289.64903703  289.48076794  289.32966917
      289.19398865  289.07215307  288.9627497   288.86450993  288.77629463
      288.69708089  288.62595019  288.56207772  288.50472285  288.45322052
      288.40697353  288.36544562  288.32815525  288.29467002  288.26460165
      288.23760148  288.21335643  288.19158537  288.17203584  288.15448116
      288.13871778  288.1245629   288.1118524   288.10043889  288.09019003
      288.08098696  288.07272299  288.06530227  288.05863878  288.05265523
      288.04728225  288.04245753  288.03812513  288.03423481  288.03074146]


Now we are going to plot this alongside the analytical solution.


```python
plt.plot(t / seconds_per_year, Tsprime, label='analytical') 
plt.plot(t / seconds_per_year, Tnumerical, label='numerical')
plt.xlabel('Years')
plt.ylabel('Global mean temperature (K)')
plt.title('Relaxation to equilibrium temperature')
plt.legend()
# the legend() function uses the labels assigned in the above plot() commands
```




    <matplotlib.legend.Legend at 0x10fa2f0d0>




![png](output_45_1.png)


So this works quite well; the two solutions look nearly identical.

Now that we have built some confidence in the numerical method, we can use it to study a slightly more complex system for which we don’t have the analytical solution.

E.g. let’s solve the full non-linear energy balance model:

$$C \frac{dT_s}{dt} = (1-\alpha) Q - \sigma \big(\beta T_s \big)^4 $$

We’ll write a new solver function:


```python
# absorbed solar is a constant in this model
ASR = (1-alpha)*Q  
# but the longwave depends on temperature... define a function for this
def OLR(Ts):
    return sigma * (beta*Ts)**4
# Now we put them together to get our simple solver function
def next_temperature_nonlinear(T0, timestep):
    return T0 + timestep/C * (ASR-OLR(T0))
```

Now solve this nonlinear model using the same procedure as above.


```python
Tnonlinear = np.zeros_like(t)
Tnonlinear[0] = Tsprime0 + Tsbar
for i in range(Tnumerical.size-1):
    Tnonlinear[i+1] = next_temperature_nonlinear(Tnonlinear[i], timestep)
```

And plot the three different solutions together:


```python
plt.plot(t / seconds_per_year, Tsprime, label='analytical') 
plt.plot(t / seconds_per_year, Tnumerical, label='numerical')
plt.plot(t / seconds_per_year, Tnonlinear, label='nonlinear')
plt.xlabel('Years')
plt.ylabel('Global mean temperature (K)')
plt.title('Relaxation to equilibrium temperature')
plt.legend()
```




    <matplotlib.legend.Legend at 0x10fc4ac90>




![png](output_51_1.png)


And we see that the models essentially do the same thing.

Now try some different initial conditions



```python
T1 = 400.  # very hot
for n in range(50):
    T1 = next_temperature_nonlinear(T1, timestep)
print T1
```

    288.294151595



```python
T1 = 200.  # very cold
for n in range(50):
    T1 = next_temperature_nonlinear(T1, timestep)
print T1
```

    287.28133982


The system relaxes back to 288 K regardless of its initial condition.

<div class="alert alert-success">
[Back to ATM 623 notebook home](../index.ipynb)
</div>

____________
## Version information
____________



```python
%load_ext version_information
%version_information numpy, matplotlib
```




<table><tr><th>Software</th><th>Version</th></tr><tr><td>Python</td><td>2.7.12 64bit [GCC 4.2.1 (Based on Apple Inc. build 5658) (LLVM build 2336.11.00)]</td></tr><tr><td>IPython</td><td>5.3.0</td></tr><tr><td>OS</td><td>Darwin 16.5.0 x86_64 i386 64bit</td></tr><tr><td>numpy</td><td>1.11.1</td></tr><tr><td>matplotlib</td><td>2.0.0</td></tr><tr><td colspan='2'>Thu May 25 13:49:24 2017 EDT</td></tr></table>



____________

## Credits

The author of this notebook is [Brian E. J. Rose](http://www.atmos.albany.edu/facstaff/brose/index.html), University at Albany.

It was developed in support of [ATM 623: Climate Modeling](http://www.atmos.albany.edu/facstaff/brose/classes/ATM623_Spring2015/), a graduate-level course in the [Department of Atmospheric and Envionmental Sciences](http://www.albany.edu/atmos/index.php)

Development of these notes and the [climlab software](https://github.com/brian-rose/climlab) is partially supported by the National Science Foundation under award AGS-1455071 to Brian Rose. Any opinions, findings, conclusions or recommendations expressed here are mine and do not necessarily reflect the views of the National Science Foundation.
____________


```python

```
