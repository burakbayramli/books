function [t,y] = odeMidpt(diffeq,tn,h,y0)
% odeMidpt   Midpoint method for integration of a single, first order ODE
%
% Synopsis:  [t,y] = odeMidpt(diffeq,tn,h,y0)
%
% Input:     diffeq = (string) name of the m-file that evaluates the right
%                     hand side of the ODE written in standard form
%            tn  = stopping value of the independent variable
%            h   = stepsize for advancing the independent variable
%            y0  = initial condition for the dependent variable
%
% Output:    t = vector of independent variable values:  t(j) = (j-1)*h
%            y = vector of numerical solution values at the t(j)

t = (0:h:tn)';           %  Column vector of elements with spacing h
n = length(t);           %  Number of elements in the t vector
y = y0*ones(n,1);        %  Preallocate y for speed
h2 = h/2;                %  Avoid repeated evaluation of this constant    

%   Begin Midpoint scheme; j=1 for initial condition
for j=2:n
   k1 = feval(diffeq,t(j-1),y(j-1));
   k2 = feval(diffeq,t(j-1)+h2,y(j-1)+h2*k1);
   y(j) = y(j-1) + h*k2;
end
