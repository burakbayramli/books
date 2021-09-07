[![Build Status](https://travis-ci.org/clawpack/riemann_book.svg?branch=master)](https://travis-ci.org/clawpack/riemann_book)

# Riemann Problems and Jupyter Solutions

#### by David I. Ketcheson, Randall J. LeVeque, and Mauricio del Razo Sarmina

This repository contains source files for a book that illustrates Riemann
solutions and approximate Riemann solvers in Jupyter notebooks.  The print version
of the book is [available from SIAM](https://my.siam.org/Store/Product/viewproduct/?ProductId=31634540),
and also as an [ebook](https://epubs.siam.org/doi/book/10.1137/1.9781611976212).  You can view a pdf 
version of the [contents, preface, and index](https://archive.siam.org/books/fa16/fa16fmbm.pdf).

    Riemann Problems and Jupyter Solutions
        Theory and Approximate Solvers for Hyperbolic PDEs
    by David I. Ketcheson, Randall J. LeVeque, and Mauricio J. del Razo
    SIAM, 2020.   ISBN: 978-1-611976-20-5
    ebook: DOI 10.1137/1.9781611976212

See [https://bookstore.siam.org/fa16/bonus](https://bookstore.siam.org/fa16/bonus) for additional 
information and links to the html rendered notebooks.
These are static views (no execution or interactive widgets), but some notebooks include animations that will play.


The recommended way to fully experience the book is by running the Jupyter notebooks:

 - [Locally, installed using Docker](#docker)
 - [Locally, installed manually](#installation)
 - [In the cloud with Binder](#binder)
 
 Start from the table of contents given in the notebook [Index.ipynb](Index.ipynb), which is also [shown below](#contents).
 The notebook [Index2.ipynb](Index2.ipynb) lists some notebooks that are not 
 in the SIAM book, some of which are still under development.  Additional notebooks may appear in the future.

## Branches
The [**FA16** branch](https://github.com/clawpack/riemann_book/tree/FA16) of this repository corresponds
to notebooks as converted into the SIAM book.  The **master** branch may be updated in the future.

## Installation
To install the [dependencies](#installation), first install a Fortran compiler.
Then do the following in a terminal:

```
pip install clawpack
git clone https://github.com/clawpack/riemann_book
cd riemann_book
pip install -r requirements.txt
jupyter nbextension enable --py widgetsnbextension
pip install jupyter_contrib_nbextensions
jupyter contrib nbextension install --user
jupyter nbextension enable equation-numbering/main
```

You can test your installation by running

```
python test.py
```

A table of contents and suggested order for reading the notebooks is given in [Index.ipynb](Index.ipynb).

If you want to compile the PDF locally, you must also install the package [bookbook](https://github.com/takluyver/bookbook).

## Docker

Rather than installing all the dependencies, if you have
[Docker](https://www.docker.com/) installed you can use

    $ docker pull clawpack/rbook

to obtain a docker image that has all the notebooks and dependencies
installed.  This was built using the [Dockerfile](Dockerfile) in
this repository, which could be modified to build a new image also
containing other material, if desired.  See [Docker.md](Docker.md) for further
instructions.

## Execute in the cloud

### Binder

Rather than installing anything on your own computer, you can run the
notebooks on the cloud using the free
[binder](https://mybinder.org/) service.  
Simply navigate to this link in a browser:

https://mybinder.org/v2/gh/clawpack/riemann_book/FA16

This may take a few minutes to start up a notebook server on a
[Jupyterhub](https://jupyterhub.readthedocs.io/en/latest/). Then navigate to
`riemann_book` and open `Index.ipynb` to get started.


## License

### Code

The code in this repository, including all code samples in the notebooks,
is released under the 3-Clause BSD License.  See
[LICENSE-CODE](https://github.com/clawpack/riemann_book/blob/master/LICENSE-CODE)
for the license and read more at the 
[Open Source Initiative](https://opensource.org/licenses/bsd-3-clause).

### Text

The text content of the notebooks is released under the CC-BY-NC-ND License.
See
[LICENSE-TEXT.md](https://github.com/clawpack/riemann_book/blob/master/LICENSE-TEXT.md)
for the license and read more at [Creative
Commons](https://creativecommons.org/licenses/by-nc-nd/4.0/).

## Contents

Taken from the notebook [Index.ipynb](Index.ipynb), these are the notebooks that also appear in the printed book:

- Preface -- Describes the aims and goals, and different ways to use the notebooks.
- Part I: The Riemann problem and its solution
  - `Introduction.ipynb` -- Introduces basic ideas with some sample solutions.
  - `Advection.ipynb` -- The scalar advection equation is the simplest hyperbolic problem.
  - `Acoustics.ipynb` -- This linear system of two equations illustrates how eigenstructure is used.
  - `Burgers.ipynb` -- The classic nonlinear scalar problem with a convex flux.
  - `Traffic_flow.ipynb` -- A nonlinear scalar problem with a nice physical interpretation.
  - `Nonconvex_scalar.ipynb` -- More interesting Riemann solutions arise when the flux is not convex.
  - `Shallow_water.ipynb` -- A classic nonlinear system of two equations.
  - `Shallow_tracer.ipynb` -- Adding a passively advected tracer and a linearly degenerate field.
  - `Euler.ipynb` -- The classic equations for an ideal gas.
- Part II: Approximate solvers
  - `Approximate_solvers.ipynb` -- Introduction to two basic types of approximations.
  - `Burgers_approximate.ipynb` -- Approximate solvers for a scalar problem.
  - `Shallow_water_approximate.ipynb` -- Roe solvers, the entropy fix, positivity, HLL, and HLLE.
  - `Euler_approximate.ipynb` -- Extension of these solvers to gas dynamics.
  - `FV_compare.ipynb` -- Comparing how different approximate solvers perform when used with PyClaw.

