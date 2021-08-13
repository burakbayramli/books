function I=l2ip(f,g,a,b,x)

%I=l2ip(f,g,a,b,x)
%
%   This function computes the L^2 inner product of two functions
%   f(x) and g(x), that is, it computes the integral from a to b of
%   f(x)*g(x).  The two functions must be defined by symbolic
%   expressions f and g.
%
%   The variable of integration is assumed to be x.  A different
%   variable can passed in as the (optional) fifth input.
%
%   The inputs a and b, defining the interval [a,b] of integration,
%   are optional.  The default values are a=0 and b=1.

% Assign the default values to optional inputs, if necessary

if nargin<5
   syms x
end

if nargin<4
   b=1;
end

if nargin<3
   a=0;
end

% Compute the integral

I=int(f*g,x,a,b);
