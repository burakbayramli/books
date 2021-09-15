# Python companion to Low Speed Aerodynamics

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow?style=flat-square)](https://opensource.org/licenses/MIT)
![Visits](https://badges.pufler.dev/visits/alwinw/katzplotkinpy?style=flat-square&label=visits)
![GitHub last commit](https://img.shields.io/github/last-commit/alwinw/katzplotkinpy?style=flat-square)

[Low Speed Aerodynamics 2nd Edition](https://www.amazon.com/Low-Speed-Aerodynamics-Second-Cambridge-Aerospace/dp/0521665523) by Joseph Katz and Allen Plotkin

## Purpose

This is a project implements the FORTRAN computer programs listed in *Low Speed Aerodynamics (2nd Ed)* into a Python package and stand alone program(s). Additional features such as visualisation have been included.

Written permission provided by Professor Joseph Katz, San Diego State University, to recreate the original FORTRAN functionality in Python and/or C for this Open Source Software.

## Programs and Features

### Programs

The following table is a summary from Appendix D of *Low Speed Aerodynamics*. Each program will be recreated in Python.

|  No.  |   Name   |                  Program Description                  | Section | Status |
| :---: | :------: | :---------------------------------------------------: | :-----: | :----: |
|       |          |                **_2D Panel Methods_**                 |         |        |
|  1.   | `AFGEN`  |    Grid generator for van de Vooren airfoil shapes    |   6.7   |   ☑    |
|       |          |         **_2D: Neumann Boundary Condition_**          |         |        |
|  2.   | `VOR2D`  |           Discrete vortex, thin wing method           | 11.1.1  |   ☐    |
|  3.   | `SOR2DC` |            Constant strength source method            | 11.2.1  |   ☐    |
|  4.   | `DUB2DC` |           Constant strength doublet method            | 11.2.2  |   ☐    |
|  5.   | `VOR2DC` |            Constant strength vortex method            | 11.2.3  |   ☐    |
|  6.   | `SOR2DL` |             Linear strength source method             | 11.4.1  |   ☐    |
|  7.   | `VOR2DL` |             Linear strength vortex method             | 11.4.2  |   ☐    |
|       |          |        **_2D: Dirichlet Boundary Condition_**         |         |        |
|  8.   | `PHICD`  |           Constant strength doublet method            | 11.3.2  |   ☐    |
|  9.   | `PHICSD` |        Constant strength source/doublet method        | 11.3.1  |   ☐    |
|  10.  | `PHILD`  |            Linear strength doublet method             | 11.5.2  |   ☐    |
|  11.  | `PHIQD`  |           Quadratic strength doublet method           | 11.6.2  |   ☐    |
|       |          |                   **_3D Programs_**                   |         |        |
|  12.  | `DUB3DC` |     Influence of constant strength source/doublet     | 10.4.1  |   ☐    |
|  13.  | `VORING` |   VLM for rectilinear surfaces (with ground effect)   |  12.3   |   ☐    |
|  14.  | `PANEL`  | Constant strength sources and doublets (Dirichlet BC) |  12.5   |   ☐    |
|       |          |             **_Time Dependent Programs_**             |         |        |
|  15.  |  `WAKE`  |   Acceleration of flat plate using a lumped vortex    |  13.7   |   ☐    |
|  16.  |  `UVLM`  | Unsteady motion of a thin rectangular lifting surface |  13.12  |   ☐    |

## Installation and Usage

### Installation

Details to come

### Usage

A program can be used as a group of programs or individually.

```plaintext
katzplotkinpy [OPTIONS] {PROGRAM NAME} [PROGRAM OPTIONS]

Python companion to Low Speed Aerodynamics

optional arguments:
  -h, --help            show this help message and exit
  -V, --version         show the version and exit

programs:
  Programs from Appendix D of 'Low Speed Aerodynamics'

  {afgen,vor2d,sor2dc,dub2dc,vor2dc,sor2dl,vor2dl,phicd,phicsd,phild,phiqd,dub3dc,voring,panel,wake,uvlm}
    afgen               Grid generator for van de Vooren airfoil shapes
    vor2d               Discrete vortex, thin wing method
    sor2dc              Constant strength source method
    dub2dc              Constant strength doublet method
    vor2dc              Constant strength vortex method
    sor2dl              Linear strength source method
    vor2dl              Linear strength vortex method
    phicd               Constant strength doublet method
    phicsd              Constant strength source/doublet method
    phild               Linear strength doublet method
    phiqd               Quadratic strength doublet method
    dub3dc              Influence of constant strength source/doublet
    voring              VLM for rectilinear surfaces (with ground effect)
    panel               Constant strength sources and doublets (Dirichlet BC)
    wake                Acceleration of flat plate using a lumped vortex
    uvlm                Unsteady motion of a thin rectangular lifting surface

Source: https://github.com/AlwinW/KatzPlotkinPy
```

```plaintext
katzplotkinpy afgen [-h] [-v] [-d] [-s]

Grid generator for van de Vooren airfoil shapes

optional arguments:
  -h, --help     show this help message and exit
  -v, --verbose  Increase log verbosity (max -vvv)
  -d, --debug    Show debugging messages (eqv. to -vv, overrides verbosity flag)
  -s, --silent   Suppress log warning and lower messages (overrides other verbosity flags)
```

### Running Tests

```sh
python3 -m unittest -v
#  or  #
coverage run -m unittest discover -v
coverage report -m
```

### Features

In addition to recreating the programs in Python, visualisation and other features have been added.

## Contributers

[@Alwin Wang](github.com/alwinw)

## Acknowledgement

Written consent given by the author to create this python port from the original Fortran code and algorithms presented in *Low Speed Aerodynamics 2nd Edition*.
