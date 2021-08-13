function I=eip(f,g,k,a,b,x)

%I=eip(f,g,k,a,b,x)
%
%   This function computes the energy inner product of two functions
%   f(x) and g(x), that is, it computes the integral from a to b of
%   k(x)*f'(x)*g'(x).  The three functions must be defined by symbolic
%   expressions f, g, and k.
%
%   The variable of integration is assumed to be x.  A different
%   variable can passed in as the (optional) sixth input.
%
%   The inputs a and b, defining the interval [a,b] of integration,
%   are optional.  The default values are a=0 and b=1.

% Assign the default values to optional inputs, if necessary

if nargin<6
   syms x
end

if nargin<5
   b=1;
end

if nargin<4
   a=0;
end

% Compute the integral

I=int(k*diff(f,x)*diff(g,x),x,a,b);
