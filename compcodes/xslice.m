

% This function fmile slices the graph of given function f with 
% a plane parallel to the yz plane.  It assumes that the function
% has already been graphed on a rectangle [a,b] times [c,d].
% The call is xslice('f',x,y0) where f is given in an mfile, and
% xslice(f, x,y0) when f is an inline function. x is a vector of
% x coordinates which ranges from a to b. y0 is the fixed value of y.


function out = xslice(f, x, y0)

    z = feval(f,x,y0);
    high = max([z+1,0]);
    low = min([z-1,0]);
    a = min(x);
    b = max(x);

    xedge = [a b b a a];
    yedge = [y0 y0 y0 y0 y0];
    zedge = [low low high high low];

    fill3(xedge, yedge, zedge,'r')
