% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function [tout, yout] = rkf45(ode_function, tspan, y0, tolerance)
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%{ 
  This function uses the Runge-Kutta-Fehlberg 4(5) algorithm to
  integrate a system of first-order differential equations
  dy/dt = f(t,y).

  y             - column vector of solutions
  f             - column vector of the derivatives dy/dt
  t             - time
  a             - Fehlberg coefficients for locating the six solution
                  points (nodes) within each time interval.
  b             - Fehlberg coupling coefficients for computing the
                  derivatives at each interior point
  c4            - Fehlberg coefficients for the fourth-order solution
  c5            - Fehlberg coefficients for the fifth-order solution
  tol           - allowable truncation error
  ode_function  - handle for user M-function in which the derivatives f
                  are computed
  tspan         - the vector [t0 tf] giving the time interval for the
                  solution
  t0            - initial time
  tf            - final time
  y0            - column vector of initial values of the vector y
  tout          - column vector of times at which y was evaluated
  yout          - a matrix, each row of which contains the components of y
                  evaluated at the correponding time in tout
  h             - time step
  hmin          - minimum allowable time step
  ti            - time at the beginning of a time step
  yi            - values of y at the beginning of a time step
  t_inner       - time within a given time step
  y_inner       - values of y witin a given time step
  te            - trucation error for each y at a given time step
  te_allowed    - allowable truncation error
  te_max        - maximum absolute value of the components of te
  ymax          - maximum absolute value of the components of y
  tol           - relative tolerance
  delta         - fractional change in step size
  eps           - unit roundoff error (the smallest number for which
                  1 + eps > 1)
  eps(x)        - the smallest number such that x + eps(x) = x

  User M-function required: ode_function
%}
% ---------------------------------------------------------------

a = [0 1/4 3/8 12/13 1 1/2];

b = [    0          0          0          0         0
        1/4         0          0          0         0
        3/32       9/32        0          0         0
     1932/2197 -7200/2197  7296/2197      0         0
      439/216      -8      3680/513   -845/4104     0
       -8/27        2     -3544/2565  1859/4104  -11/40];

c4 = [25/216  0  1408/2565    2197/4104   -1/5    0  ];
c5 = [16/135  0  6656/12825  28561/56430  -9/50  2/55]; 

if nargin < 4
    tol  = 1.e-8;
else
    tol = tolerance;
end

t0   = tspan(1);
tf   = tspan(2);
t    = t0;
y    = y0;
tout = t;
yout = y';
h    = (tf - t0)/100; % Assumed initial time step.

while t < tf
    hmin = 16*eps(t);
    ti   = t;
    yi   = y;
    %...Evaluate the time derivative(s) at six points within the current
    %   interval:
    for i = 1:6
        t_inner = ti + a(i)*h;
        y_inner = yi;
        for j = 1:i-1
            y_inner = y_inner + h*b(i,j)*f(:,j);
        end
        f(:,i) = feval(ode_function, t_inner, y_inner);
    end

    %...Compute the maximum truncation error:
    te     = h*f*(c4' - c5'); % Difference between 4th and
                              % 5th order solutions
    te_max = max(abs(te));    
   
    %...Compute the allowable truncation error:
    ymax       = max(abs(y));
    te_allowed = tol*max(ymax,1.0);
    
    %...Compute the fractional change in step size:
    delta = (te_allowed/(te_max + eps))^(1/5);
     
    %...If the truncation error is in bounds, then update the solution:
    if te_max <= te_allowed
        h     = min(h, tf-t);
        t     = t + h;
        y     = yi + h*f*c5';      
        tout  = [tout;t];
        yout  = [yout;y'];
    end
    
    %...Update the time step:
    h  = min(delta*h, 4*h);
    if h < hmin
        fprintf(['\n\n Warning: Step size fell below its minimum\n'...
                 ' allowable value (%g) at time %g.\n\n'], hmin, t)
        return
    end  
end
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
