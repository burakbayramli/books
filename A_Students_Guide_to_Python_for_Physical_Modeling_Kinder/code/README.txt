Use of these code fragments is subject to the terms of LICENSE.txt.

CONTENTS:
------------------------------------------------------------------------- 
string_format.py
------------------------------------------------------------------------- 
Illustrate string formatting using the .format() method.

------------------------------------------------------------------------- 
string_percent.py
------------------------------------------------------------------------- 
Illustrate string formatting using the % method.

------------------------------------------------------------------------- 
for_loop.py
------------------------------------------------------------------------- 
Use a for loop to generate multiple solutions to the quadratic equation.

This script illustrates the funamental form of a for loop.  For alternate
solutions to the same problem, see while_loop.py and vectorize.py.

------------------------------------------------------------------------- 
while_loop.py
------------------------------------------------------------------------- 
Use a while loop to generate multiple solutions to the quadratic equation.

For alternate solutions to the same problem, see for_loop.py and
vectorize.py.

------------------------------------------------------------------------- 
vectorize.py
------------------------------------------------------------------------- 
Use vectorized operations to generate multiple solutions to the quadratic
equation.

For alternate solutions to the same problem, see for_loop.py and
while_loop.py.

------------------------------------------------------------------------- 
projectile.py
------------------------------------------------------------------------- 
Calculate how long an object is in the air when thrown from a spcified
height with a range of initial speeds assuming constant acceleration due
to gravity:
	0.5 * g * t**2 - v0 * t - y0 = 0

This script illustrates good coding practice in the solution of a simple
problem: parameters with descriptive names, comments, whitespace, and
blocking with '#%%' for debugging in Spyder.

-------------------------------------------------------------------------
branching.py
------------------------------------------------------------------------- 
This script illustrates branching with the use of multiple conditional
statements:
	if <condition1>:
		...
	elif <condition2>:
		...
	else:
		...

------------------------------------------------------------------------- 
nesting.py
------------------------------------------------------------------------- 
Use nested for loops to fill a two-dimensional array of values.

This script illustrates "nesting"---one for loop inside of another.

------------------------------------------------------------------------- 
import_text.py
------------------------------------------------------------------------- 
Load data from a text file by reading the file line by line.

This script reads in data from a text file and stores it in a NumPy array.
It can be adapted to load data from files that are difficult or impossible
to load with NumPy's np.loadtxt function.

------------------------------------------------------------------------- 
save_load.py
------------------------------------------------------------------------- 
Save array data using NumPy's available methods, then load saved data.

This script demonstrates the various methods of saving and loading data
using NumPy arrays.

------------------------------------------------------------------------- 
perrin.py
------------------------------------------------------------------------- 
Generate figure displaying Perrin's experimental data on Brownian motion.
This script requires the data set 04brownian/g26perrindata.npy.

The script illustrates loading and plotting a data set and includes
LaTeX formatting of axis labels and grid lines.

You must copy g26perrindata.npy into the current folder to run the script.

------------------------------------------------------------------------- 
print_write.py
------------------------------------------------------------------------- 
Write same data to a file and print to display.

This script illustrates the similarites between writing to a text file and
printing to the screen.

------------------------------------------------------------------------- 
simple_plot.py
------------------------------------------------------------------------- 
Create and display a basic plot.

------------------------------------------------------------------------- 
graph_modifications.py
------------------------------------------------------------------------- 
This script creates a simple plot with two lines, then modifies several
features of the plot, including axis labels, labels and legend, line
style, tick labels, and title.

------------------------------------------------------------------------- 
line3d.py
------------------------------------------------------------------------- 
Create a three-dimensional parametric plot.

This script demonstrates how to create three-dimensional plots using the
Axes3D method from the mpl_toolkits.mplot3d module.  See also surface.py.

------------------------------------------------------------------------- 
subplots.py
------------------------------------------------------------------------- 
Create four plots in the same figure.

This script demonstrates PyPlot's subplot method, which can be used to
display several plots side-by-side in the same figure.

------------------------------------------------------------------------- 
rotate.py
------------------------------------------------------------------------- 
Define function to rotate a vector in two dimensions.

------------------------------------------------------------------------- 
average.py
-------------------------------------------------------------------------
Compute and return the cummulative average of an array.

This script illustrates the principles of functional programming.

------------------------------------------------------------------------- 
histogram.py
-------------------------------------------------------------------------
Create histograms of random numbers.

This script illustrates how to use NumPy and PyPlot to create histograms
and bar plots.

------------------------------------------------------------------------- 
contour.py
-------------------------------------------------------------------------
Create a labeled contour plot.

This script illustrates how to generate a grid of coordinates for contour
and surface plots.  It also demonstrates some options of plt.contour and
shows how to label contour lines.

------------------------------------------------------------------------- 
matrix_inversion.py
------------------------------------------------------------------------- 
Invert a simple matrix to solve a system of linear equations.

This script illustrates the use of a special method from the SciPy linear
algebra library, scipy.linalg.

------------------------------------------------------------------------- 
quadrature.py
------------------------------------------------------------------------- 
Integrate two functions using quad.

This script demonstrates numerical integration using the quad method of
scipy.integrate.  The first function is a built-in NumPy funciton whose
integral can be computed with pencil and paper for comparison.  The second
is a user-defined function.

------------------------------------------------------------------------- 
simple_oscillator.py
------------------------------------------------------------------------- 
Define function to use in solution of differential equation for a simple
harmonic oscillator.

This script illustrates how to write a function that generates the array
required to integrate a second-order ordinary differential equation.  It
is imported and used in solve_ode.py.

------------------------------------------------------------------------- 
solve_ode.py
------------------------------------------------------------------------- 
Solution of ODE for harmonic oscillator.

This script imports the function F(y,t) in simple_oscillator.py then uses
the odeint method of scipy.integrate to solve the ordinary differential
equation defined by F(y,t).

------------------------------------------------------------------------- 
parametric_oscillator.py
------------------------------------------------------------------------- 
Define a parametric function that accepts 4 parameters then integrate it
using odeint.

This script illustrates two methods for using scipy.integrate's odeint
methods to integrate a function that accepts more than two parameters.

------------------------------------------------------------------------- 
quiver.py
------------------------------------------------------------------------- 
Create a quiver plot.

This script illustrates the use of PyPlot's quiver method.

------------------------------------------------------------------------- 
gradient.py
------------------------------------------------------------------------- 
Calculate and display the gradient of a two-dimensional Gaussian.

This script illustrates the use of NumPy's gradient function and
demonstrates how to display a vector field.  It displays the gradient as a
quiver plot superimposed on a filled contour plot of the Gaussian.

------------------------------------------------------------------------- 
streamlines.py
------------------------------------------------------------------------- 
Create streamlines from a vector field.

This script demonstrates the use of PyPlot's streamplot method for
visualizing solutions to a differential equation defined by a vector
field.

------------------------------------------------------------------------- 
walker.py
------------------------------------------------------------------------- 
Make a movie out of the steps of a two-dimensional random walk.

This script demonstrates the use of the FuncAnimation method of
Matplotlib's animation module to create a movie.  If ffmpeg or mencoder is
installed on this computer, the script will save the movie to an mp4 file.

------------------------------------------------------------------------- 
html_movie.py
------------------------------------------------------------------------- 
Module to generate an HTML document from a collection of images.  When
viewed in a Web browser, the document will display a movie whose frames
are the individual images.

This module is adapted from the scitools library developed by Hans Petter
Langtangen.

------------------------------------------------------------------------- 
waves.py
------------------------------------------------------------------------- 
Create an HTML animation of a moving Gaussian waves.

This script illustrates a method for combining a series of plots into an
animation using HTML and Javascript.  It uses the html_movie.py module,
which is  adapted from the scitools library developed by Hans Petter
Langtangen.

------------------------------------------------------------------------- 
sympy_examples.py
------------------------------------------------------------------------- 
Demonstrate some useful methods available in the SymPy module.

Python may complain about undefined variables if you attempt to run this
script.  init_session() defines several variables, but Python may not be
aware of this.  It is better to run the commands one at a time from the
command line.

------------------------------------------------------------------------- 
convolution.py
------------------------------------------------------------------------- 
This script creates an eLoG (elongated Laplacian of Gaussian) filter that
emphasizes long, vertical lines in a figure.  The effect of the filter is
demonstrated on a plus sign.

------------------------------------------------------------------------- 
scope.py
-------------------------------------------------------------------------
Demonstrate Python's rules of scope.

------------------------------------------------------------------------- 
name_collision.py
-------------------------------------------------------------------------
Illustrate how Python's rules of scope prevent name collisions.

------------------------------------------------------------------------- 
fancy_plot.py
-------------------------------------------------------------------------
Add a title and axis labels to a simple plot.

------------------------------------------------------------------------- 
legend.py
-------------------------------------------------------------------------
Create a plot with a legend to distinguish multiple curves.

------------------------------------------------------------------------- 
measurements.py
------------------------------------------------------------------------- 
Functions to calculate distance between points using different metrics.

This script illustrates the fundamental form of user-defined functions as
well as keyword arguments and default values.

------------------------------------------------------------------------- 
random_walk.py
------------------------------------------------------------------------- 
Monte Carlo simulation of a two-dimensional random walk.

This script illustrates the use of a random number generator to create a
time series for a random walk.

------------------------------------------------------------------------- 
surface.py
------------------------------------------------------------------------- 
Create a three-dimensional surface plot.

This script demonstrates how to create three-dimensional plots using the
Axes3D method from the mpl_toolkits.mplot3d module.  See also line3d.py.

------------------------------------------------------------------------- 
surprise.py
------------------------------------------------------------------------- 
This script will create a familar but interesting image.
It may take about a minute to run.
