# lbm_matlab
Lattice Boltzmann Method (LBM) codes in MATLAB for educational purposes. 
I developed these as a feature-wise precursor to my more serious C++ version.  

IMPORTANT NOTE: The velocities in the default navier stokes versions are set 
to 1, which leads to breakdown of the incompressible assumption. You will need 
to set the velocities to proper values. Please see my c++ version for a more 
readily validated version.  

The features I explore here in the Navier-Stokes code are (see the references 
section for titles and more info):  

* Arbitrary refinement: (2005 Chen et al.) and (2012 Chen). 
  Multiple levels of refinement in the same grid.
* Arbitrary shape wall boundary boundary condition: (2011 Li) and 
  (1998 Chen et al.).
  A cut-cell volumetric wall boundary condition. As a volumetric BC, MME (mass, 
  momentum and energy) are perfectly conserved, unlike the interpolation 
  approaches.
* Viscosity counteraction: (2015 Zhang et al.) and (2011 Cheng et al.).
  A surprisingly effective and simple way to achieve high numerical stability, 
  allowing for orders-of-magnitude higher Reynolds number stability limits.
* Multiple relaxation time (MRT): (2011 Mohamad) and (2015 Zhang et al.).
  A slightly more computationally expensive but significantly more stable than 
  the commonly-implemented single relaxation time (SRT) method. It is noted 
  that there seems to be inconsistencies between the (2011 Mohamad) code and 
  textbook explanantions, namely the definition of the vector of moments. 
* RANS Spalart-Allmaras turbulence model: (2015 Pellerin et al.).
  Simple inclusion of RANS eddy viscosity turbulence into the relaxation time 
  for the assumption of fully-turbulent flow. 

## Contents

### navier_stokes

The 'navier_stokes' folder contains 2DQ9 codes for various physical 
configurations. There are several matlab scripts to be run that demonstrate 
different individual solver features. Below are explanations of each runnable 
script.  

#### Lid-Driven Cavity Scripts

* cavity_cut: features the arbitrary shape wall BC by clipping the domain of 
  the conventional lid-driven cavity case.
* cavity_dim: a modification of the (2011 Mohamad) code port to be more 
  organized with respect to dimensionalization; see (2008 Latt).
* cavity_interface: features two levels of grid refinement, with the interface 
  along the vertical center of the cavity.
* cavity_mohamad: a port of the code provided by (2011 Mohamad). 
* cavity_mrt: MRT version.
* cavity_mrt_vcs: MRT version with the viscosity-counteracting approach 
  (steady approximation). For details on the steady approximation, see 
  (2015 Zhang et al.). About the force term (which is implicit), 
  they suggest an explicit form good enough for near-steady-state. I use an 
  even simpler formulation that does not use spatially adjacent terms; purely 
  local. Seems to still work okay. 
* cavity_sa: SRT with Spalart-Allmaras turbulence model.

#### Simple Channel Scripts

* channel_ext0: simple channel with a zero-gradient outlet condition. It is 
  tehnically low-order accurate (first, I think), but it is personally my 
  favorite due to its stability.
* channel_ext2: simple channel with a second-order zero-gradient outlet 
  condition. I have found it to be unstable.
* channel_fixedp: simple channel with a zero-gradient pressure outlet 
  condition. It is based on the explanation in (2012 Mohamad). I have 
  found it to be unstable.
* channel_fixedv: simple channel with a fixed velocity at the outlet. 
  It is almost trivial, since it is not really a physically sound outlet 
  condition.
* channel_interface: simple channel with a zero-gradient outlet condition 
  (as implemented in channel_ext0). It also features an interface where the 
  cell resolution changes midway through the channel. Based on the theory of 
  (2005 Chen et al.) and (2012 Chen).

#### Other Scripts

* inlet_outlet: an almost trivial code where all boundaries are either an 
inlet or outlet, faciliatating an inclined freestream flow.  

### advection_diffusion and diffusion

Based on codes in (2012 Mohamad) for simple 1d and 2d diffusion 
and advection-diffusion problems. Each script within these folders can be run 
on its own.  

## References

* (1998) Hudong Chen, Chris Teixeira, and Kim Molvig. 
  REALIZATION OF FLUID BOUNDARY CONDITIONS VIA DISCRETE BOLTZMANN DYNAMICS.

* (2011) Yanbing Li. 
  AN IMPROVED VOLUMETRIC LBM BOUNDARY APPROACH AND ITS EXTENSION FOR SLIDING 
  MESH SIMULATION.

* (2005) Hudong Chen, O. Filippova, J. Hoch, Kim Molvig, R. Shock, 
  Chris Teixiera, R. Zhang. 
  GRID REFINEMENT IN LATTICE BOLTZMANN METHODS BASED ON VOLUMETRIC FORMULATION.

* (2008) Latt, J. 
  CHOICE OF UNITS IN LATTICE BOLTZMANN SIMULATIONS.

* (2011) A. A. Mohamad.
  LATTICE BOLTZMANN METHOD: FUNDAMENTALS AND ENGINEERING APPLICATIONS WITH 
  COMPUTER CODES.

* (2011) Yongguang Cheng and Hui Zhang. 
  A VISCOSITY COUNTERACTING APPROACH IN THE LATTICE BOLTZMANN BGK MODEL FOR LOW 
  VISCOSITY FLOW: PRELIMINARY VERIFICATION.

* (2012) Xiao-Peng Chen. 
  APPLICATIONS OF LATTICE BOLTZMANN METHOD TO TURBULENT FLOW AROUND 
  TWO-DIMENSIONAL AIRFOIL.

* (2015) Chunze Zhang, Yongguang Cheng, Shan Huang, and Jiayang Wu. 
  IMPROVING THE STABILITY OF THE MULTIPLE-RELAXATION-TIME LATTICE BOLTZMANN 
  METHOD BY A VISCOSITY COUNTERACTING APPROACH. 

* (2015) Nicolas Pellerin, Sebastien Leclaire, and Marcelo Reggio. 
  AN IMPLEMENTATION OF THE SPALART–ALLMARAS TURBULENCE MODEL IN A MULTI-DOMAIN 
  LATTICE BOLTZMANN METHOD FOR SOLVING TURBULENT AIRFOIL FLOWS.



