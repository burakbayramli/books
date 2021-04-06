[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.4647887.svg)](https://doi.org/10.5281/zenodo.4647887)

Solving the advection-diffusion equation on an non-uniform mesh with
the finite-volume method

*********************************************************************************************


This repo is basically my notes on learning the finite-volume method
when applied to the advection-diffusion equation. The methods are
based on the the book by W. Hundsdorfer and J. G. Verwer, [Numerical
solutions of time-dependent advection-diffusion reaction
equations](https://books.google.co.uk/books?isbn=3540034404).

For my lab-book notes on solving the advection-diffusion equation with
the finite-volume method see the accompanying gh-pages branch,
[https://danieljfarrell.github.io/FVM/index.html](https://danieljfarrell.github.io/FVM/index.html).

The code is written in python using numpy as scipy libraries.

Ideas I wanted to explore:

 * How to implement a cell centred mesh?
 * How to implement a non-uniform mesh?
 * How to include adaptive upwinding?
  - This means that the discretisation automatically adjusts to an appropriate scheme depending on the local value of the Peclet number.
  - I use exponentially fitting which is a nice method because it enables to equations to be solved for *any* Peclet number.
  - A central difference scheme is limited to Peclet number < 2, however a upwind scheme the Peclet number is unbounded.
  - Please not that the CFL condition imposes a limited on the time step when using upwind method.
 * How to include Robin boundary conditions which do not allow any out flow.
 * How to include Dirichlet boundary conditions.
 * How to export a movie of time-dependent simulations.
 
 
  
  
  
