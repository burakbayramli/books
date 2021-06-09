function [x,y] = odeRK4v(diffeq,xn,h,y0,varargin)
% odeRK4v  Fourth order Runge-Kutta method for a single, first order ODE
%          Vectorized version with pass-through parameters.
%
% Synopsis:  [x,y] = odeRK4v(fun,xn,h,y0)
%            [x,y] = odeRK4v(fun,xn,h,y0,arg1,arg2,...)
%
% Input:     diffeq = (string) name of the m-file that evaluates the right
%                     hand side of the ODE written in standard form
%            xn  = stopping value of the independent variable
%            h   = stepsize for advancing the independent variable
%            y0  = initial condition for the dependent variable
%            arg1,arg2 = list of additional arguments that are passed through
%                        odeRK4v to the ``diffeq'' routine.
%
% Output:    x = vector of independent variable values:  x(j) = (j-1)*h
%            y = vector of numerical solution values at the x(j)

x = (0:h:xn)';                   %  Column vector of elements with spacing h
n = length(x);                   %  Number of elements in the x vector
y = y0*ones(n,1);                %  Preallocate y for speed
h2 = h/2;  h3 = h/3;  h6 = h/6;  %  Avoid repeated evaluation of constants    
flag = 'odeRK4v';                %  Dummy parameter passed to `diffeq'

%  Begin RK4 integration; j=1 for initial condition
for j=2:n
   k1 = feval(diffeq, x(j-1),    y(j-1)      , flag,varargin{:});
   k2 = feval(diffeq, x(j-1)+h2, y(j-1)+h2*k1, flag,varargin{:});
   k3 = feval(diffeq, x(j-1)+h2, y(j-1)+h2*k2, flag,varargin{:});
   k4 = feval(diffeq, x(j-1)+h,  y(j-1)+h*k3 , flag,varargin{:});
   y(j) = y(j-1) + h6*(k1+k4) + h3*(k2+k3);
end
