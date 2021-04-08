# CFD Basics
Coding practice of Anderson's CFD book: __Computational Fluid Dynamics: The basics with applications__

## Laval (Chapter7)
The Laval pipe, a classical 1D problem, based on Euler equation.
### (0) Subsonic-Supersonic Isentropic Flow
MacCormack Scheme.  
Usage:
> * Compile: `g++ main.cc -o Laval`
> * Execute: `./Laval`
> * Animate: `python3 animate.py`

The program will produce a flowfield history file named `flow.txt`, and the steady-state flowfield looks like:  
![steady-laval](Laval/0/steady.png)

Pay attention to B.C. at both inlet and outlet!

### (1) Subsonic Isentropic Flow
MacCormack Scheme.  
Clearly, velocity peaks at central.  
Usage:
> * Compile: `g++ main.cc -o Laval`
> * Execute: `./Laval`
> * Animate: `python3 animate.py`

### (2) Conservative form for Subsonic-Supersonic Isentropic Flow
MacCormack Scheme.  
Usage:
> * Compile: `g++ main.cc -o Laval`
> * Execute: `./Laval`
> * Animate: `python3 animate.py`

### (3) Shockwave Capture
MacCormack Scheme.  
Add artificial viscosity at both prediction and correction steps.  
Usage:
> * Compile: `g++ main.cc -o Laval`
> * Execute: `./Laval`
> * Animate: `python3 animate.py`

The program will produce a flowfield history file named `flow.txt`, and the steady-state flowfield looks like:  
![steady-shock](Laval/3/steady.png)

## Couette (Chapter8)
Viscous flow between 2 parallel plate.  
### (0) 1D simulation
The simplified G.E. is given as:  

<div align=center><img src="Couette/1D/eqn.png"/></div>

it is similiar with unsteady heat transfer equation, which is __parabolic__.  
Crank-Nicolson method is used, which is __unconditionally__ stable due to its implicitness. Hence, larger time-step can be taken via tuning the parameter `E`.  
However, errors during iteration will get larger when `E` is increasing due to larger truncation error.  
This well illustrates that, even with implict scheme, timestep can not go to infinity!(an optimal timestep in between)

Usage:
> * Compile: `g++ main.cc -o Couette`
> * Execute: `./Couette`
> * Animate: `python3 animate.py`

The program will produce a flowfield history file named `flow.txt`, and the steady-state flowfield(with `E=1.0`) looks like:  

<div align=center><img src="Couette/1D/steady.png"/></div>

Be careful with the index inside the Thomas algorithm!

### (1) 2D simulation
Pressure-Correction method in general.  
Classical schemes like __SIMPLE__, __SIMPLER__ and __PISO__ are used on __staggered__ grids.  
Grid with virtual nodes is adopted as illustrated in `Chapter 8.4.1`.  
Variable placement follows the convention introduced in `Chapter 6.8.4`.  

<div align=center><img src="Couette/2D/grid.png"/></div>

Standard TECPLOT ASCII data files will be produced every time-step.

#### (1.1) SIMPLE
Standard SIMPLE method is used to achieve final steady-state result.  
The poisson equation is solved implicitly by solving a linear system.  
Convergency history:

|<div align=center><img src="Couette/2D/SIMPLE/mass_flux.png"/></div>|<div align=center><img src="Couette/2D/SIMPLE/u.png"/></div>|
|:-:|:-:|
|mass flux at (15, 5)|__u__ at i=15|

Values on __Boundary__:

-|Left Inlet|Right Outlet|Top Lid|Bottom Wall
:-:|:-:|:-:|:-:|:-:
p'|zero|zero|zero-gradient|zero-gradient
u|linear extrapolation|linear extrapolation|Ue|0
v|0|by computation|0|0

Values on __virtual__ nodes are mostly calculated by __linear extrapolation__ from neighbouring nodes.  

Usage:
> * Compile: `g++ main.cc -std=c++14 -I /usr/include/eigen3 -o Couette`
> * Execute: `./Couette`
> * View full flowfield: `Tecplot` or `ParaView` or `EnSight`
> * Animate convergency history at (15, 5): `python3 animate.py`
> * Path of `Eigen3` may vary in different systems or platforms, adjust it accordingly.

#### (1.2) SIMPLER
It is similiar with __SIMPLE__ in general, but a better __p*__ is provided by calculating the pressure equation in advance within each iteration loop.  
Convergency history:

|<div align=center><img src="Couette/2D/SIMPLER/u.png"/></div>|<div align=center><img src="Couette/2D/SIMPLER/v.png"/></div>|
|:-:|:-:|
|__u__ at i=15 | __v__ at i=15|

Usage:
> * Compile: `g++ main.cc -std=c++14 -I /usr/include/eigen3 -o Couette`
> * Execute: `./Couette`
> * View full flowfield: `Tecplot` or `ParaView` or `EnSight`
> * Animate convergency history at (15, 5): `python3 animate.py`
> * Path of `Eigen3` may vary in different systems or platforms, adjust it accordingly.

__SIMPLER__ is much more stable than __SIMPLE__ in terms of the divergence term.

#### (1.3) PISO
In my opinion, __PISO__ corrects pressure twice, while __SIMPLER__ predicts once and corrects once.  
Convergency history:

|<div align=center><img src="Couette/2D/PISO/u.png"/></div>|<div align=center><img src="Couette/2D/PISO/v.png"/></div>|
|:-:|:-:|
|__u__ at i=15 | __v__ at i=15|

Usage:
> * Compile: `g++ main.cc -std=c++14 -I /usr/include/eigen3 -o Couette`
> * Execute: `./Couette`
> * View full flowfield: `Tecplot` or `ParaView` or `EnSight`
> * Animate convergency history at (15, 5): `python3 animate.py`
> * Path of `Eigen3` may vary in different systems or platforms, adjust it accordingly.

Pay attention to the B.C. of the pressure correction equation!  
It seems unstable in the begining, may be improved if under-relaxation is used when updating pressure.

## Plate (Chapter9)
Supersonic flow over a plate.  
2 different thermal B.C. are examined.

### (0) Isothermal B.C.
Wall temperature is fixed.  
MacCormack scheme is adopted.  
The back-and-forth alternation on derivatives aims at obtaining 2nd-order accuracy.  

Flowfiled at Steady-State:

|<div align=center><img src="Plate/0-Isothermal/fig/U.png"/></div>|<div align=center><img src="Plate/0-Isothermal/fig/V.png"/></div>|
|:-:|:-:|
|<div align=center><img src="Plate/0-Isothermal/fig/p.png"/></div>|<div align=center><img src="Plate/0-Isothermal/fig/T.png"/></div>|

Pressure at bottom:

<div align=center><img src="Plate/0-Isothermal/fig/p_bottom.png"/></div>

Values at outlet:

<div align=center><img src="Plate/0-Isothermal/fig/outlet.png"/></div>

Standard TECPLOT data file in ASCII format will be produced every 100 steps.  
An aditional file `history.txt` will be written for animation.

Usage:
> * Compile: `g++ main.cc -std=c++14 -o Plate -O3`
> * Execute: `./Plate`
> * View: `Tecplot` or `ParaView` or `EnSight`
> * Animate: `python3 animate.py`

Pay attention to values at ___boundary___, remember to update them!

### (1) Adiabatic B.C.
Similar to previous, only wall temperature gradient is set to 0.  
This is achieved by simply setting `T(i, JMIN) = T(i, JMIN+1)` inside program.  

Flowfiled at Steady-State:

|<div align=center><img src="Plate/1-Adiabatic/fig/U.png"/></div>|<div align=center><img src="Plate/1-Adiabatic/fig/V.png"/></div>|
|:-:|:-:|
|<div align=center><img src="Plate/1-Adiabatic/fig/p.png"/></div>|<div align=center><img src="Plate/1-Adiabatic/fig/T.png"/></div>|

Pressure at bottom:

<div align=center><img src="Plate/1-Adiabatic/fig/p_bottom.png"/></div>

Values at outlet:

<div align=center><img src="Plate/1-Adiabatic/fig/outlet.png"/></div>

Standard TECPLOT data file in ASCII format will be produced every 100 steps.  
An aditional file `history.txt` will be written for animation.  

Usage:
> * Compile: `g++ main.cc -std=c++14 -o Plate -O3`
> * Execute: `./Plate`
> * View: `Tecplot` or `ParaView` or `EnSight`
> * Animate: `python3 animate.py`
