% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function Example_1_20
% ~~~~~~~~~~~~~~~~~~~
%{
  This program uses RKF4(5) with adaptive step size control
  to solve the differential equation
  
  x'' + mu/x^2 = 0

  The numerical integration is done by the function 'rkf45' which uses
  the subfunction 'rates' herein to compute the derivatives.

  x     - displacement (km)
  '     - shorthand for d/dt
  t     - time (s)
  mu    - = go*RE^2 (km^3/s^2), where go is the sea level gravitational
          acceleration and RE is the radius of the earth.
  x0    - initial value of x
  v0    = initial value of the velocity (x')
  y0    - column vector containing x0 and v0
  t0    - initial time
  tf    - final time
  tspan - a row vector with components t0 and tf
  t     - column vector of the times at which the solution is found
  f     - a matrix whose columns are:
          column 1: solution for x at the times in t
          column 2: solution for x' at the times in t
  
  User M-function required:  rkf45
  User subfunction required: rates
%}
% ---------------------------------------------------------------------

clear all; close all; clc

mu      = 398600;
minutes = 60;  %Conversion from minutes to seconds

x0 = 6500;
v0 = 7.8;
y0 = [x0; v0];
t0 = 0;
tf = 70*minutes;

[t,f] = rkf45(@rates, [t0 tf], y0);
plotit
return

% ~~~~~~~~~~~~~~~~~~~~~~~~
function dfdt = rates(t,f)
% ------------------------
%{
  This function calculates first and second time derivatives of x
  governed by the equation of two-body rectilinear motion

  x'' + mu/x^2 = 0
 
  Dx   - velocity x'
  D2x  - acceleration x''
  f    - column vector containing x and Dx at time t
  dfdt - column vector containing Dx and D2x at time t

  User M-functions required: none
%}
% ~~~~~~~~~~~~~~~~~~~~~~~~
x    = f(1);
Dx   = f(2);
D2x  = -mu/x^2;
dfdt = [Dx; D2x];
end %rates

% ~~~~~~~~~~~~~
function plotit
% ~~~~~~~~~~~~~

%...Position vs time:
subplot(2,1,1)
plot(t/minutes,f(:,1), '-ok')
xlabel('time, minutes')
ylabel('position, km')
grid on
axis([-inf inf 5000 15000])

%...Velocity versus time:
subplot(2,1,2)
plot(t/minutes,f(:,2), '-ok')
xlabel('time, minutes')
ylabel('velocity, km/s')
grid on
axis([-inf inf -10 10])
end %plotit

end %Example_1_20
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~