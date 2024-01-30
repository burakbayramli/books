<div align="center">
  <img src="https://github.com/JWock82/PyNite/raw/main/Resources/Full Logo No Buffer.png" width=40% align="center"/>
  <br>
  <h1>Simple Finite Element Analysis in Python</h1>
</div>

![Build Status](https://github.com/JWock82/PyNite/actions/workflows/build-and-test.yml/badge.svg)
![PyPI - Downloads](https://img.shields.io/pypi/dm/PyNiteFEA)
<img alt="GitHub code size in bytes" src="https://img.shields.io/github/languages/code-size/JWock82/PyNite">
![GitHub last commit](https://img.shields.io/github/last-commit/JWock82/PyNite)
![GitHub](https://img.shields.io/github/license/JWock82/PyNite)
[![Documentation Status](https://readthedocs.org/projects/pynite/badge/?version=latest)](https://pynite.readthedocs.io/en/latest/?badge=latest)

An easy to use elastic 3D structural engineering finite element analysis library for Python.

# Installation
The easiest way to install Pynite is with pip: `pip install PyniteFEA`

For a more detailed discussion on installation options and dependencies see https://pynite.readthedocs.io/en/latest/installation.html

# Current Capabilities
* 3D static analysis of elastic structures.
* P-&Delta; analysis of frame type structures.
* Member point loads, linearly varying distributed loads, and nodal loads are supported.
* Classify loads by load case and create load combinations from load cases.
* Produces shear, moment, and deflection results and diagrams for each member.
* Automatic handling of internal nodes on frame members (physical members).
* Tension-only and compression-only elements.
* Spring elements: two-way, tension-only, and compression-only.
* Spring supports: two-way and one-way.
* Quadrilateral plate elements (MITC4 formulation).
* Rectangular plate elements (12-term polynomial formulation).
* Basic meshing algorithms for some common shapes and for openings in rectangular walls.
* Reports support reactions.
* Rendering of model geometry, supports, load cases, load combinations, and deformed shapes.
* Generates PDF reports for models and model results.

# Project Objectives
As I've gotten into the structural engineering profession, I've found there's a need for an easy to use open-source finite element package. I hope to help fill that need by prioritizing the following:

1. Accuracy: There are no guarantees PyNite is error free, but accuracy and correctness are a priority. When bugs or errors are identified, top priority will be given to eliminate them. PyNite's code is frequently reviewed, and its output is tested against a suite of textbook problems with known solutions using continuous integration (CI) anytime a change to the code base is made. If you do happen to find an error, please report it as an issue.

2. Simplicity: There are other finite element alternatives out there with many more capabilities, but they are often lacking in documentation, written in difficult languages, or require extensive knowledge of finite element theory and/or element formulations to use. PyNite is not intended to be the most technically advanced solver out there. Rather, the goal is to provide a robust yet simple general purpose package.

4. Improvement: PyNite is getting better at what it does. Each new feature provides leverage to build upon previous features in more elaborate ways. Key to improvement is (a) maintaining the core features that other features rely on, and (b) adding new features that are solid stepping stones to other features. Improvement most often happens by getting the small and simple things right incrementally, rather than with sweeping overhauls all at once.

5. Collaboration: If you see a way to improve Pynite, you are encouraged to contribute. There are many simple ways to contribute that don't take much effort. Issue reports and pull requests can be very helpful. One easy way to contribute is to add to or improve the library of simple example problems in the `Examples` folder. Please keep them relatively simple. Most users learn Pynite from following these simple examples. Another way to help Pynite without having to know too much about its internal workings is to help with the documentation files in the `docs\source` folder of this repository. These files help new users learn Pynite. If you are able to make a bigger commitment, and would like to become a regular contributor to the project, please reach out about becoming a collaborator.

# Support
Whether you just need help getting started with PyNite, or are looking to build something more complex, there are a few resources available:
* The examples in the "Examples" folder in this repository cover a variety of simple problems. The comments in the examples provide additional guidance on how PyNite works.
* Documentation is a work in progress and can be found on readthedocs here: https://pynite.readthedocs.io/en/latest/index.html.
* If you're looking for more direct guidance on using PyNite, or for help coding a project, I am available on a private consulting basis. You can reach out to me directly at Building.Code@outlook.com to discuss options.

# Example Projects
Here's a list of projects that use PyNite:

* Building Code (https://building-code.herokuapp.com/) - This one is my personal side project.
* Standard Solver (https://www.standardsolver.com/)
* Civils.ai (https://civils.ai/1/free-3D-finite-element-structural-analysis)
* Phaenotyp (https://github.com/bewegende-Architektur/Phaenotyp) (https://youtu.be/shloSw9HjVI)

# What's New?
v0.0.87
* Fine-tuned P-$\delta$ effects. P-$\delta$ effects are now included in member internal slope and deflection calculations.

v0.0.85
* Changed member moment diagrams to no longer include P-little-delta effects in non-P-Delta analyses. This leads to consistent moment diagrams across members for simple analysis methods. Before, even when P-Delta effects weren't being calculated, P-little-delta effects were included in member moment diagrams, leading to member moment diagrams that didn't quite match up across joints on non-P-Delta models with significant P-Delta effects.

v0.0.83 & v0.0.84
* Fixed a bug in P-Delta analysis member moment calculations. Global results were correct, but member internal moments were neglecting the P-little delta effect. The result was correct moments at nodal locations, but incorrect results in between. This issue has been resolved.

v0.0.82
* `Sections` have been introduced to allow for member stresses to be tracked by the program during nonlinear analysis in the future. This opens the door for other useful features down the line too. Use of `Sections` is optional, and will only required for pushover analysis when that feature is implemented. You do not need to use this feature yet.
* P-Delta analysis code has been greatly simplified. Performance has also been improved, as redundant iterations are no longer being performed. Removed the `tol` parameter as it is unecessary when using a geometric stiffness matrix instead of an iterative procedure.
* Better documentation for P-Delta analysis.
* Corrections to unit tests that weren't working properly. Added another AISC Benchmark unit test for P-Delta analysis. This should help safeguard the program against some types of bugs being introduced going forward.
* Bug fix for multiple support springs at a single node. When calculating reactions, the program was only considering the effects of one spring at the node, whichever came first in this list: DX, DY, DZ, RX, RY, RZ. This only affected reaction calculations, and has been remedied.
* Bug fix for mesh max/min results. The program was throwing exceptions when the user requested results from the mesh rather than the elements themselves.
* Work has begun on nonlinear pushover analysis. This has proven to be a challenging feature to implement, and still has a long way to go. However, in the process of working on pushover analaysis several inneficiencies in the analysis code were identified and fixed. This update includes those changes, and brings the "work-in-progress" pushover code into the main branch of the project. The  pushover branch into to the main branch because critical parts of the analysis code were diverging from the main branch, making it harder to accept pull requests from other users.

v0.0.80
* Refactored/simplified analysis code. Much of it has been moved to a new `Analysis` file that eliminated redundant code.
* Load combination tags have replaced `combo_type`. You can now use a list of tags to tag your load combinations for easier categorization.
* You no longer have to run all load combinations. You can now run select combos based on their tags.
* Started basic documentation for plates.

v0.0.79
* Added the option to turn off nodes during visualization.
* Bug fix for meshing cylinders about the global X or Z axis.

v0.0.78
* Corrections to tension/compression only support springs. v0.0.76 and v0.0.77 were not working as expected. 3rd time's a charm.

v0.0.77
* Simplified P-Delta convergence checks
* Fixed P-Delta bug introduced in v0.0.76
  
v0.0.76
* Simplified P-Delta convergence checks
* Allowed tension/compression-only support springs to reactivate after being deactivated. Erroneous deflections were being reported on very flexible models that experienced a lot of movement with T/C support springs.
* `matplotlib` is now an optional dependency. You'll only need to install it if you want to view plots for members.
* Documentation for installation has been improved.
