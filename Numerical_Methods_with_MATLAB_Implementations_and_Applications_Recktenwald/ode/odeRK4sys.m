function [t,y] = odeRK4sys(diffeq,tn,h,y0)
% odeRK4sys  Fourth order Runge-Kutta method for systems of first order ODEs
%            Nonvectorized version
%
% Synopsis:  [t,y] = odeRK4sys(diffeq,tn,h,y0)
%
% Input:     diffeq = (string) name of the m-file that evaluates the right
%                      hand side of the ODE system written in standard form.
%            tn      = stoping value of the independent variable
%            h       = stepsize for advancing the independent variable
%            y0      = vector of the dependent variable values at t = 0
%
% Output:    t = vector of independent variable values:  t(j) = (j-1)*h
%            y = matrix of dependent variables values, one column for each
%                state variable.  Each row is from a different time step.

t = (0:h:tn)';         %  Column vector of elements with spacing h
nt = length(t);        %  number of steps
neq = length(y0);      %  number of equations that are simultaneously advanced
y = zeros(nt,neq);     %  Preallocate y for speed
y(1,:) = y0(:)';       %  Assign IC. y0(:) is column, y0(:)' is row vector

h2 = h/2;  h3 = h/3;  h6 = h/6;   %  Avoid repeated evaluation of constants    
k1 = zeros(neq,1);  k2 = k1;      %  Preallocate memory for the Runge-Kutta
k3 = k1;  k4 = k1;  ytemp = k1;   %  coefficients and a temporary vector

%  Outer loop for all steps:  j = time step index;  n = equation number index 
for j=2:nt
   told = t(j-1);   yold = y(j-1,:)';   %  Temp variables, yold is column vector
   k1 = feval(diffeq,told,yold);        %  Slopes at the starting point
   for n=1:neq
     ytemp(n) = yold(n) + h2*k1(n);     %  Estimate all y's at midpoint
   end
   k2 = feval(diffeq,told+h2,ytemp);    %  1st estimate of slopes at midpoint
   for n=1:neq
     ytemp(n) = yold(n) + h2*k2(n);     %  2nd estimate of all y's at midpoint
   end
   k3 = feval(diffeq,told+h2,ytemp);    %  2nd estimate of slopes at midpoint
   for n=1:neq
     ytemp(n) = yold(n) + h*k3(n);      %  Estimate y at end point
   end
   k4 = feval(diffeq,told+h,ytemp);     %  Estimate of slopes at endpoint
   for n=1:neq                          %  Simultaneously advance all y's
     y(j,n) = yold(n) + h6*(k1(n)+k4(n)) + h3*(k2(n)+k3(n));
   end
end
