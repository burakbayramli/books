function n = peak_pick( x );
%
%	FUNCTION 	n = peak_pick( x )
%
%	The function peak_pick() returns the indices of the peaks of the
%		row vector x.
%
lenx = length(x);
xnm1 = x(1:lenx-2);
xn = x(2:lenx-1);
xnp1 = x(3:lenx);
n = find( (xn>xnm1) & (xn>xnp1) );
n = n + 1;
