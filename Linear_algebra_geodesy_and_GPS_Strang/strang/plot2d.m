function plot2d(X)
%PLOT2D	Two dimensional plot.
%	X is matrix with two rows and any number of columns.
%	PLOT2D(X) plots these columns as points in the plane
%	and connects them with lines.
%	The scale is set to [-10,10] in both directions.
%
%	For example, the statements
%           hand
%	or
%	    house
%	create sample X's representing common figures.
%	Then, for various 2 by 2 matrices A,
%	    plot2d(A*X)
%	demonstrates the effect of multiplication by A.
%	   
x = X(1,:)';
y = X(2,:)';
plot(x,y,'go',x,y,'b-');
axis([-10 10 -10 10])
axis('square')
