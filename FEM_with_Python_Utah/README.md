                            FEM with Python

What is it?
===========

FEM with Python is a collection of course notes, assignments, projects, etc.
that I developed for teaching an introductory course on the Finite Element
Method at the University of Utah. As the name implies, materials are targeted
for learning the finite element method using the
[Python](https://www.python.org) programming language.

License
=======

All course materials are licensed under the MIT License:

> Copyright (c) 2014 Tim Fuller

> Permission is hereby granted, free of charge, to any person obtaining a copy
> of this software and associated documentation files (the "Software"), to deal
> in the Software without restriction, including without limitation the rights
> to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
> copies of the Software, and to permit persons to whom the Software is
> furnished to do so, subject to the following conditions:

> The above copyright notice and this permission notice shall be included in
> all copies or substantial portions of the Software.

> THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
> IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
> FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
> AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
> LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
> OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
> THE SOFTWARE.

Materials
=========

Course materials are written and distributed as
[IPython notebooks](http://ipython.org/notebook.html) and can be viewed online
at [NBViewer](http://nbviewer.ipython.org/github/tjfulle/fem-with-python/tree/master)

Requirements
============

All course materials require a Python distribution with the `numpy`, `scipy`, `matplotlib`, `traits`, and `chaco` packages installed. You can try to install each of these packages on your systems standard Python distribution (not recommended), or download and install the [Anaconda Community Edition of Python](https://store.continuum.io/cshop/anaconda) or [Enthought Canopy](https://store.enthought.com).

Obtaining Course Materials
==========================

Course materials are maintained under version control using
[git](http://git-scm.com).

According to Wikepedia,

> Git is a distributed revision control and source code management (SCM)
> system with an emphasis on speed. Git was initially designed and developed
> by Linus Torvalds for Linux kernel development in 2005.

There are several ways to obtain course materials

Obtaining Course Materials with GIT
-----------------------------------

Mac OS X and Linux have `git` installed out of the box. On Windows, binary
installers are available at http://msysgit.github.com/.

Once `git` is installed, obtain class materials by opening a terminal session
(command prompt) and navigating to the directory you would like the materials
to reside.  There, execute

`git clone https://github.com/tjfulle/fem-with-python`

You now have full access to all course materials.

Alternatives to Cloning the Repository
--------------------------------------

If you don't have `git` installed and don't want to install it, you can simply
download the course materials by clicking on the `Download ZIP` button at
https://github.com/tjfulle/fem-with-python. Note, if you do this, you
cannot update using `git pull` as described below and must re-download when
new materials are published.

Updating the Course Materials
-----------------------------

As course materials are updated, you can obtain the updates by simply navigating to the directory where the materials are already cloned and executing

`git pull`
