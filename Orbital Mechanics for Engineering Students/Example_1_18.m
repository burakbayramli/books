% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function Example_1_18
% ~~~~~~~~~~~~~~~~~~~
%{
  This function uses the RK1 through RK4 methods with two 
  different time steps each to solve for and plot the response
  of a damped single degree of freedom spring-mass system to
  a sinusoidal forcing function, represented by

  x'' + 2*z*wn*x' + wn^2*x = (Fo/m)*sin(w*t)

  The numerical integration is done by the external
  function 'rk1_4', which uses the subfunction 'rates'
  herein to compute the derivatives.

  This function also plots the exact solution for comparison.

  x           - displacement (m)
  '           - shorthand for d/dt
  t           - time (s)
  wn          - natural circular frequency (radians/s)
  z           - damping factor
  wd          - damped natural frequency
  Fo          - amplitude of the sinusoidal forcing function (N)
  m           - mass (kg)
  w           - forcing frequency (radians/s)
  t0          - initial time (s)
  tf          - final time (s)
  h           - uniform time step (s)
  tspan       - a row vector containing t0 and tf
  x0          - value of x at t0 (m)
  x_dot0      - value of dx/dt at t0 (m/s)
  f0          - column vector containing x0 and x_dot0
  rk          - = 1 for RK1; = 2 for RK2; = 3 for RK3; = 4 for RK4
  t           - solution times for the exact solution
  t1, ...,t4  - solution times for RK1,...,RK4 for smaller 
  t11,...,t41 - solution times for RK1,...,RK4 for larger h
  f1, ...,f4  - solution vectors for RK1,...,RK4 for smaller h
  f11,...,f41 - solution vectors for RK1,...,RK4 for larger h

  User M-functions required:  rk1_4
  User subfunctions required: rates
%}
% ------------------------------------------------------------------------

clear all; close all; clc

%...Input data:
m      = 1;
z      = 0.03;
wn     = 1;
Fo     = 1;
w      = 0.4*wn;

x0     = 0;
x_dot0 = 0;
f0     = [x0; x_dot0];

t0     = 0;
tf     = 110;
tspan  = [t0 tf];
%...End input data

%...Solve using RK1 through RK4, using the same and a larger
%   time step for each method:
rk = 1;
h  = .01; [t1,  f1]  = rk1_4(@rates, tspan, f0, h, rk);
h  = 0.1; [t11, f11] = rk1_4(@rates, tspan, f0, h, rk);

rk = 2;
h  = 0.1; [t2,  f2]  = rk1_4(@rates, tspan, f0, h, rk);
h  = 0.5; [t21, f21] = rk1_4(@rates, tspan, f0, h, rk);

rk = 3;
h  = 0.5; [t3,  f3]  = rk1_4(@rates, tspan, f0, h, rk);
h  = 1.0; [t31, f31] = rk1_4(@rates, tspan, f0, h, rk);

rk = 4;
h  = 1.0; [t4,  f4]  = rk1_4(@rates, tspan, f0, h, rk);
h  = 2.0; [t41, f41] = rk1_4(@rates, tspan, f0, h, rk);

output

return

% ~~~~~~~~~~~~~~~~~~~~~~~~
function dfdt = rates(t,f)
% ------------------------
%{
  This function calculates first and second time derivatives
  of x as governed by the equation

  x'' + 2*z*wn*x' + wn^2*x = (Fo/m)*sin(w*t)

  Dx   - velocity (x')
  D2x  - acceleration (x'')
  f    - column vector containing x  and Dx  at time t
  dfdt - column vector containing Dx and D2x at time t

  User M-functions required: none
%}
% ~~~~~~~~~~~~~~~~~~~~~~~~

x    = f(1);
Dx   = f(2);
D2x  = Fo/m*sin(w*t) - 2*z*wn*Dx - wn^2*x;
dfdt = [Dx; D2x];
end %rates

% ~~~~~~~~~~~~~
function output
% -------------
%...Exact solution:
wd  = wn*sqrt(1 - z^2);
den = (wn^2 - w^2)^2 + (2*w*wn*z)^2;
C1  = (wn^2 - w^2)/den*Fo/m;
C2  = -2*w*wn*z/den*Fo/m;
A   = x0*wn/wd + x_dot0/wd +(w^2 + (2*z^2 - 1)*wn^2)/den*w/wd*Fo/m;
B   = x0 + 2*w*wn*z/den*Fo/m;

t   = linspace(t0, tf, 5000);
x   = (A*sin(wd*t) + B*cos(wd*t)).*exp(-wn*z*t) ...
      + C1*sin(w*t) + C2*cos(w*t);
   
%...Plot solutions
%   Exact:
subplot(5,1,1)
plot(t/max(t),     x/max(x),               'k',  'LineWidth',1)
grid off
axis tight
title('Exact')

%   RK1:
subplot(5,1,2)
plot(t1/max(t1),   f1(:,1)/max(f1(:,1)),   '-r', 'LineWidth',1)
hold on
plot(t11/max(t11), f11(:,1)/max(f11(:,1)), '-k')
grid off
axis tight
title('RK1')
legend('h = 0.01', 'h = 0.1')

%   RK2:
subplot(5,1,3)
plot(t2/max(t2),   f2(:,1)/max(f2(:,1)),   '-r', 'LineWidth',1)
hold on
plot(t21/max(t21), f21(:,1)/max(f21(:,1)), '-k')
grid off
axis tight
title('RK2')
legend('h = 0.1', 'h = 0.5')

%   RK3:
subplot(5,1,4)
plot(t3/max(t3),   f3(:,1)/max(f3(:,1)),   '-r', 'LineWidth',1)
hold on
plot(t31/max(t31), f31(:,1)/max(f31(:,1)), '-k')
grid off
axis tight
title('RK3')
legend('h = 0.5', 'h = 1.0')

%   RK4:
subplot(5,1,5)
plot(t4/max(t4),   f4(:,1)/max(f4(:,1)),   '-r', 'LineWidth',1)
hold on
grid off
plot(t41/max(t41), f41(:,1)/max(f41(:,1)), '-k')
axis tight
title('RK4')
legend('h = 1.0', 'h = 2.0')
end %output

end %Example_1_18
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       