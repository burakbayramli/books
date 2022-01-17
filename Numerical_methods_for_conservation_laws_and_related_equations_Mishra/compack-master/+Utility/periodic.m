function y = periodic( x, range )
%CLAMP   Force x periodically into the interval [range(1), range(2)].
%   If you want initial data f(x) with periodic boundary over the domain 
%   'range', set the initial data to be
%		initial = @(x) f(periodic(x, range));
%   Exact solvers in the Error package will then "see" the periodicity of
%   the boundary, without knowing anything about the boundary condition
%   object.
%
%   Arguments:
%   x:		A vector of points that are to be restricted to an interval.
%   range:	The interval to restrict to. Must be a vector of length 2
%			specifying the lower and upper boundaries of the interval.

	a = range(1);
	b = range(2);
	y = mod(x-a, b-a)+a;

end