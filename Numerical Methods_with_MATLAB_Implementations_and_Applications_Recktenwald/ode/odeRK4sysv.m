function [t,y] = odeRK4sysv(diffeq,tn,h,y0,varargin)
% odeRK4sysv  Fourth order Runge-Kutta method for systems of first order ODEs
%             Vectorized version with pass-through parameters.
%
% Synopsis:  [t,y] = odeRK4sysv(diffeq,tn,h,y0)
%            [t,y] = odeRK4sysv(diffeq,tn,h,y0,arg1,arg2,...)
%
% Input:     diffeq = (string) name of the m-file that evaluates the right
%                      hand side of the ODE system written in standard form.
%            tn      = stoping value of the independent variable
%            h       = stepsize for advancing the independent variable
%            y0      = vector of the dependent variable values at x = x0
%            arg1,arg2 = list of additional arguments passed through
%                        odeRK4sysv to the ``diffeq'' routine.
%
% Output:    t = vector of independent variable values:  t(j) = x0 + j*h
%            y = matrix of dependent variables values, one column for each
%                state variable.  Each row is from a different time step.

t = (0:h:tn)';           %  Column vector of elements with spacing h
nt = length(t);          %  number of steps (+1 for the initial conditions)
neq = length(y0);        %  number of equations simultaneously advanced
y = zeros(nt,neq);       %  Preallocate y for speed
y(1,:) = y0(:)';         %  Assign IC. y0(:) is column, y0(:)' is row vector
flag = [];               %  Dummy argument for compatibility with rhs functions
                         %  developed for built-in routines

h2 = h/2;  h3 = h/3;  h6 = h/6;   %  Avoid repeated evaluation of constants    
k1 = zeros(neq,1);  k2 = k1;      %  Preallocate memory for the Runge-Kutta
k3 = k1;  k4 = k1;  ytemp = k1;   %  coefficients and a temporary vector

%  Outer loop for all steps:  j = time step index;  k = equation number index
%  Note use of transpose on definition of yold, and in formula for y(j,:) 
for j=2:nt
   told = t(j-1);   yold = y(j-1,:)';                 %  Temp variables
   k1 = feval(diffeq,told,yold,flag,varargin{:});     %  Slopes at the start
   ytemp = yold + h2*k1;
   k2 = feval(diffeq,told+h2,ytemp,flag,varargin{:}); %  1st slope at midpoint
   ytemp = yold + h2*k2;
   k3 = feval(diffeq,told+h2,ytemp,flag,varargin{:}); %  2nd slope at midpoint
   ytemp = yold + h*k3;
   k4 = feval(diffeq,told+h,ytemp,flag,varargin{:});  %  Slope at endpoint
   y(j,:) = ( yold + h6*(k1+k4) + h3*(k2+k3) )';      %  Advance all equations
end
