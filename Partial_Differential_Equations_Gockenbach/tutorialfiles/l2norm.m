function I=l2norm(f,a,b)

%I=l2norm(f,a,b,x)
%
%   This function computes the L^2 inner product of the function f(x)
%   The functions must be defined by the symbolic expressions f.
%
%   The variable of integration is assumed to be x.  A different
%   variable can passed in as the (optional) fourth input.
%
%   The inputs a and b, defining the interval [a,b] of integration,
%   are optional.  The default values are a=0 and b=1.

% Assign the default values to optional inputs, if necessary

if nargin<4
   syms x
end

if nargin<3
   b=1;
end

if nargin<2
   a=0;
end

% Compute the integral

I=sqrt(int(f^2,x,a,b));
