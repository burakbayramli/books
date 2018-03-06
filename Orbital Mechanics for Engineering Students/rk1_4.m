% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function [tout, yout] = rk1_4(ode_function, tspan, y0, h, rk)
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%{ 
  This function uses a selected Runge-Kutta procedure to integrate
  a system of first-order differential equations dy/dt = f(t,y).

  y             - column vector of solutions
  f             - column vector of the derivatives dy/dt
  t             - time
  rk            - = 1 for RK1; = 2 for RK2; = 3 for RK3; = 4 for RK4
  n_stages      - the number of points within a time interval that
                  the derivatives are to be computed
  a             - coefficients for locating the solution points within
                  each time interval
  b             - coefficients for computing the derivatives at each
                  interior point
  c             - coefficients for the computing solution at the end of
                  the time step
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
  ti            - time at the beginning of a time step
  yi            - values of y at the beginning of a time step
  t_inner       - time within a given time step
  y_inner       - values of y within a given time step
  
  User M-function required: ode_function
%}
% ------------------------------------------------------------------------

%...Determine which of the four Runge-Kutta methods is to be used:
switch rk
    case 1
        n_stages = 1;
        a = 0;
        b = 0;
        c = 1;
    case 2
        n_stages = 2;
        a = [0 1];
        b = [0 1]';
        c = [1/2 1/2];
    case 3
        n_stages = 3;
        a = [0 1/2 1];
        b = [ 0  0 
             1/2 0
             -1  2];
        c = [1/6 2/3 1/6];
    case 4
        n_stages = 4;
        a = [0 1/2 1/2 1];
        b = [ 0   0   0
             1/2  0   0
              0  1/2  0
              0   0   1];
        c = [1/6 1/3 1/3 1/6];
    otherwise
         error('The parameter  rk  must have the value 1, 2, 3 or 4.')          
end

t0   = tspan(1);
tf   = tspan(2);
t    = t0;
y    = y0;
tout = t;
yout = y';

while t < tf
    ti = t;
    yi = y;
    %...Evaluate the time derivative(s) at the 'n_stages' points within the
    %   current interval:
    for i = 1:n_stages
        t_inner = ti + a(i)*h;
        y_inner = yi;
        for j = 1:i-1
            y_inner = y_inner + h*b(i,j)*f(:,j);
        end
        f(:,i)  = feval(ode_function, t_inner, y_inner);
    end

    h    = min(h, tf-t);
    t    = t + h;
    y    = yi + h*f*c';               
    tout = [tout;t];  % adds t to the bottom of the column vector tout
    yout = [yout;y']; % adds y' to the bottom of the matrix yout
end

end
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~