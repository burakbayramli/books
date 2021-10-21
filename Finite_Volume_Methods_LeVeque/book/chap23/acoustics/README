
2d acoustics with a radially-symmetric solution.

Solved on a quadrilateral grid using the mapping specified in 
mapc2p.f.  This is currently set to cover a chevron-shaped region with a
grid that is not orthogonal and not smooth along one line.

See setaux.f for a description of how grid information is stored
in the aux array.   Variable coefficients for the acoustics equation are
also allowed, though in this example the they are constant and set in
setprob.f.

The data is a smooth radial hump specified in qinit.f
The 1d equation with source term is solved in subdirectory 1drad
to create an accurate reference solution.

If using plotclaw2.m to plot in matlab, make sure MappedGrid=1 is set and
that the mapping function specified in mapc2p.m agrees with what is in
mapc2p.f.

After running 2d code, with same set of output times as in 1drad,
you can compare a scatter plot of the 2d results with the 1d results by
setting PlotType=4 in plotclaw2.

WARNING: This example was modified February, 2006 to use
Version 4.3 of clawpack and the associated matlab scripts.

