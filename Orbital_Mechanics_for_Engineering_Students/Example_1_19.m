% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function Example_1_19
% ~~~~~~~~~~~~~~~~~~~
%{
  This program uses Heun's method with two different time steps to solve
  for and plot the response of a damped single degree of freedom
  spring-mass system to a sinusoidal forcing function, represented by
 
  x'' + 2*z*wn*x' + wn^2*x = (Fo/m)*sin(w*t)

  The numerical integration is done in the external function 'heun',
  which uses the subfunction 'rates' herein to compute the derivatives.

  x     - displacement (m)
  '     - shorthand for d/dt
  t     - time (s)
  wn    - natural circular frequency (radians/s)
  z     - damping factor
  Fo    - amplitude of the sinusoidal forcing function (N)
  m     - mass (kg)
  w     - forcing frequency (radians/s)
  t0    - initial time (s)
  tf    - final time (s)
  h     - uniform time step (s)
  tspan - row vector containing t0 and tf
  x0    - value of x at t0 (m)
  Dx0   - value of dx/dt at t0 (m/s)
  f0    - column vector containing x0 and Dx0
  t     - column vector of times at which the solution was computed
  f     - a matrix whose columns are:
          column 1: solution for x at the times in t
          column 2: solution for x' at the times in t
 
  User M-functions required:  heun
  User subfunctions required: rates
%}
% ----------------------------------------------------------------------

clear all; close all; clc

%...System properties:
m       = 1;
z       = 0.03;
wn      = 1;
Fo      = 1;
w       = 0.4*wn;

%...Time range:
t0      = 0;
tf      = 110;
tspan   = [t0 tf];

%...Initial conditions:
x0  = 0;
Dx0 = 0;
f0  = [x0; Dx0];

%...Calculate and plot the solution for h = 1.0:
h        = 1.0;
[t1, f1] = heun(@rates, tspan, f0, h);

%...Calculate and plot the solution for h = 0.1:
h        = 0.1;
[t2, f2] = heun(@rates, tspan, f0, h);

output

return

% ~~~~~~~~~~~~~~~~~~~~~~~~
function dfdt = rates(t,f)
% ~~~~~~~~~~~~~~~~~~~~~~~~
%
% This function calculates first and second time derivatives of x
% for the forced vibration of a damped single degree of freedom
% system represented by the 2nd order differential equation
%
% x'' + 2*z*wn*x' + wn^2*x = (Fo/m)*sin(w*t)
%
% Dx   - velocity
% D2x  - acceleration
% f    - column vector containing x and Dx at time t
% dfdt - column vector containing Dx and D2x at time t
%
% User M-functions required: none
% -------------------------
x    = f(1);
Dx   = f(2);
D2x  = Fo/m*sin(w*t) - 2*z*wn*Dx - wn^2*x;
dfdt = [Dx; D2x];
end %rates

% ~~~~~~~~~~~~~
function output
% ~~~~~~~~~~~~~
plot(t1, f1(:,1), '-r', 'LineWidth',0.5)
xlabel('time, s')
ylabel('x, m')
grid
axis([0 110 -2 2])
hold on
plot(t2, f2(:,1), '-k', 'LineWidth',1)
legend('h = 1.0','h = 0.1')       
end %output

end %Example_1_19
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~