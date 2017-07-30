
# [ATM 623: Climate Modeling](../index.ipynb)
[Brian E. J. Rose](http://www.atmos.albany.edu/facstaff/brose/index.html), University at Albany
# Lecture 1: Planetary energy budget

### About these notes:

This document uses the interactive [`Jupyter notebook`](https://jupyter.org) format. The notes can be accessed in several different ways:

- The interactive notebooks are hosted on `github` at https://github.com/brian-rose/ClimateModeling_courseware
- The latest versions can be viewed as static web pages [rendered on nbviewer](http://nbviewer.ipython.org/github/brian-rose/ClimateModeling_courseware/blob/master/index.ipynb)
- A complete snapshot of the notes as of May 2017 (end of spring semester) are [available on Brian's website](http://www.atmos.albany.edu/facstaff/brose/classes/ATM623_Spring2017/Notes/index.html).

[Also here is a legacy version from 2015](http://www.atmos.albany.edu/facstaff/brose/classes/ATM623_Spring2015/Notes/index.html).

Many of these notes make use of the `climlab` package, available at https://github.com/brian-rose/climlab

## Contents

1. [What is a Climate Model?](#section1)
2. [The observed global energy budget](#section2)
3. [Quantifying the planetary energy budget](#section3)

____________
<a id='section1'></a>

## 1. What is a Climate Model?
____________

Let's be a little pedantic and decompose that question:

- what is Climate?
- what is a Model?

**Climate** is

- statistics of weather, e.g. space and time averages of temperature and precip.
- (statistics might also mean higher-order stats: variability etc)

A **model** is

 - not easy to define!

Wikipedia: http://en.wikipedia.org/wiki/Conceptual_model

> In the most general sense, a model is anything used in any way to represent anything else. Some models are physical objects, for instance, a toy model which may be assembled, and may even be made to work like the object it represents. Whereas, a conceptual model is a model made of the composition of concepts, that thus exists only in the mind. Conceptual models are used to help us know, understand, or simulate the subject matter they represent.

George E. P. Box (statistician):
> Essentially, all models are wrong, but some are useful.”

From the Climate Modelling Primer, 4th ed (McGuffie and Henderson-Sellers):

> In the broadest sense, models are for learning about the world (in our case, the climate) and the learning takes place in the contruction and the manipulation of the model, as anyone who has watched a child build idealised houses or spaceships with Lego, or built with it themselves, will know.  Climate models are, likewise, idealised representations of a complicated and complex reality through which our understanding of the climate has significantly expanded. All models involve some ignoring, distoring and approximating, but gradually they allow us to build understanding of the system being modelled. A child's Lego construction typically contains the essential elements of the real objects, improves with attention to detail, helps them understand the real world, but is never confused with the real thing.

### A minimal definition of a climate model

*A representation of the exchange of energy between the Earth system and space, and its effects on average surface temperature.*

(what average?) 

Note the focus on **planetary energy budget**. That’s the key to all climate modeling.

____________
<a id='section2'></a>

## 2. The observed global energy budget
____________

The figure below shows current best estimates of the *global, annual mean* energy fluxes through the climate system.

We will look at many of these processes in detail throughout the course.


```python
from IPython.display import Image
Image('../images/GlobalEnergyBudget.png')
```




![png](output_12_0.png)



## Things to note:

### On the shortwave side

- global mean albedo is 101.9 W m$^{-2}$ / 341.3 W m$^{-2}$ = 0.299
- Reflection off clouds = 79 W m$^{-2}$
- Off surface = 23 W m$^{-2}$
    - 3 times as much reflection off clouds as off surface
    
Why??  Think about both areas of ice and snow, and the fact that sunlight has to travel through cloudy atmosphere to get to the ice and snow. Also there is some absorption of shortwave by the atmosphere.

- Atmospheric absorption = 78 W m$^{-2}$
(so about the same as reflected by clouds)

QUESTION: Which gases contribute to shortwave absorption?

- O$_3$ and H$_2$O mostly.
- We will look at this later.

### On the longwave side

- Observed emission from the SURFACE is 396 W m$^{-2}$
- very close to the blackbody emission $\sigma T^4$ at $T = 288$ K (the global mean surface temperature).
- BUT emission to space is much smaller = 239 W m$^{-2}$

QUESTION: What do we call this?  (greenhouse effect)

### Look at net numbers…

- Net absorbed = 0.9 W m$^{-2}$
- Why?
- Where is that heat going?

Note, the exchanges of energy between the surface and the atmosphere are complicated, involve a number of different processes. We will look at these more carefully later.

### Additional points:

- Notice that this is a budget of energy, not temperature.
- We will need to discuss the connection between the two
- **Clouds** affect both longwave and shortwave sides of the budget.
- **WATER** is involved in many of the terms: 

    - evaporation
    - latent heating (equal and opposite in the global mean)
    - clouds
    - greenhouse effect
    - atmospheric SW absorption
    - surface reflectivity (ice and snow)

### Discussion point

How might we expect some of the terms in the global energy budget to vary under anthropogenic climate change?

____________
<a id='section3'></a>

## 3. Quantifying the planetary energy budget
____________

A budget for the **energy content of the global atmosphere-ocean system**:

\begin{align} 
\frac{dE}{dt} &= \text{net energy flux in to system} \\
 &= \text{flux in – flux out}
\end{align}

where $E$ is the **enthalpy** or **heat content** of the total system.

We will express the budget **per unit surface area**, so each term above has units W m$^{-2}$

Note: any **internal exchanges** of energy between different reservoirs (e.g. between ocean, land, ice, atmosphere) do not appear in this budget – because $E$ is the **sum of all reservoirs**.

### Assumption:

**The only quantitatively important energy sources to the whole system are radiative fluxes to and from space.**

Let’s model those TOA (top-of-atmosphere) fluxes.

Flux in is **incoming solar radiation**
The solar constant is

$$ S_0 = 1365.2 \text{ W m}^{-2}  $$

(all values will be consistent with Trenberth and Fasullo figure unless noted otherwise)

This is the flux of energy from the sun incident on a unit area perpendicular to the beam direction.

The area-weighted global mean incoming solar flux is

$$ Q = S_0 \frac{A_{cross-section}}{A_{surface}}  $$

[ draw sketch of sphere and illuminated disk ] 

where 

- $A_{cross-section}$ = area of the illuminated disk = $\pi a^2$
- $A_{surface}$ = surface area of sphere = $4 \pi a^2$
- $a$ = radius of Earth

So flux in is $Q =  S_0 / 4 = 341.3$ W m$^{-2}$

Flux out has two parts:

- Reflected solar radiation
- Emitted terrestrial (longwave) radiation

Introduce terminology / notation:

**OLR = outgoing longwave radiation = terrestrial emissions to space**

Define the **planetary albedo**:

- $\alpha$ = reflected solar flux / incoming solar flux
- Or reflected flux = $\alpha Q$ = 101.9 W m$^{-2}$ from data
- So from data, $\alpha \approx 0.3$

Define **ASR = absorbed solar radiation**
\begin{align}
ASR &= \text{ incoming flux – reflected flux} \\
 &= Q - \alpha Q \\
 &= (1-\alpha) Q 
\end{align}

Our energy budget then says

$$ \frac{dE}{dt} = (1-\alpha) Q - OLR $$

Note: **This is a generically true statement.** We have just defined some terms, and made the [very good] assumption that the only significant energy sources are radiative exchanges with space.

**This equation is the starting point for EVERY CLIMATE MODEL.**

But so far, we don’t actually have a MODEL. We just have a statement of a budget. To use this budget to make a model, we need to relate terms in the budget to state variables of the atmosphere-ocean system.

For now, the state variable we are most interested in is **temperature** – because it is directly connected to the physics of each term above.


____________
<a id='section4'></a>

## 4. The simplest climate model: 
## The zero-dimensional energy balance model (EBM)

____________

Suppose the Earth behaves like a **blackbody radiator** with effective global mean **emission temperature $T_e$**.

Then

$$ OLR = \sigma T_e^4 $$

with $\sigma = 5.67 \times 10{-8}$ W m$^{-2}$ K$^{-4}$ the Stefan-Boltzmann constant

**We can just take this as a definition of the emission temperature.**

Also suppose that 

$$ E = C T_s $$

where $T_s$ is the **global mean surface temperature**, and $C$ is a constant – the **effective heat capacity** of the atmosphere- ocean column.

$C$ is in units of J m$^{-2}$ K$^{-1}$.

Why? We will look at this more carefully later, but essentially because the **internal energy of a fluid is proportional to its temperature**. Water and air heat up when you add energy to them!

We parameterize $E$ in terms of the **surface temperature** mostly because *most of the heat capacity is in the oceans*. 

Now our budget is

$$C \frac{d T_s}{dt} = (1-\alpha) Q - \sigma T_e^4$$

**The climate system is at equilibrium when $ASR = OLR$** and the time derivative goes to zero.

This occurs when the emission temperature is precisely
$$ \overline{T_e} = \bigg( \frac{(1-α)Q}{σ}\bigg)^{\frac{1}{4}} $$

with the overline denoting equilibrium.

Putting in numbers from our observed budget, we find $\overline{T_e} = 255$ K

- If the Earth emits as a blackbody at 255 K, the global energy budget is balanced and there will be no change in surface temperature
- If the Earth emits at a higher [lower] blackbody temperature, the atmosphere-ocean system will lose [gain] energy and the surface will cool [warm].

Notice that our time-dependent budget is a **single equation in two unknowns, $T_s$ and $T_e$**.

For now, we will assume that $C$, $\alpha$ and $Q$ are all fixed – though we will discuss reasons why all these terms vary throughout this course.

<div class="alert alert-success">
[Back to ATM 623 notebook home](../index.ipynb)
</div>

____________
## Version information
____________



```python
%load_ext version_information
%version_information
```




<table><tr><th>Software</th><th>Version</th></tr><tr><td>Python</td><td>2.7.12 64bit [GCC 4.2.1 (Based on Apple Inc. build 5658) (LLVM build 2336.11.00)]</td></tr><tr><td>IPython</td><td>5.3.0</td></tr><tr><td>OS</td><td>Darwin 16.5.0 x86_64 i386 64bit</td></tr><tr><td colspan='2'>Thu May 25 13:49:59 2017 EDT</td></tr></table>



____________

## Credits

The author of this notebook is [Brian E. J. Rose](http://www.atmos.albany.edu/facstaff/brose/index.html), University at Albany.

It was developed in support of [ATM 623: Climate Modeling](http://www.atmos.albany.edu/facstaff/brose/classes/ATM623_Spring2015/), a graduate-level course in the [Department of Atmospheric and Envionmental Sciences](http://www.albany.edu/atmos/index.php)

Development of these notes and the [climlab software](https://github.com/brian-rose/climlab) is partially supported by the National Science Foundation under award AGS-1455071 to Brian Rose. Any opinions, findings, conclusions or recommendations expressed here are mine and do not necessarily reflect the views of the National Science Foundation.
____________


```python

```
