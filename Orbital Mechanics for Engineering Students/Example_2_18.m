% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function Example_2_18
% ~~~~~~~~~~~~~~~~~~~
%{
  This program uses the Runge-Kutta-Fehlberg 4(5) method to solve the
  earth-moon restricted three-body problem (Equations 2.192a and 2.192b)
  for the trajectory of a spacecraft having the initial conditions
  specified in Example 2.18.

  The numerical integration is done in the external function 'rkf45',
  which uses the subfunction 'rates' herein to compute the derivatives.

  days      - converts days to seconds
  rmoon     - radius of the moon (km)
  rearth    - radius of the earth (km)
  r12       - distance from center of earth to center of moon (km)
  m1,m2     - masses of the earth and of the moon, respectively (kg)
  M         - total mass of the restricted 3-body system (kg)
  mu        - gravitational parameter of earth-moon system (km^3/s^2)
  mu1,mu2   - gravitational parameters of the earth and of the moon,
              respectively (km^3/s^2)
  pi_1,pi_2 - ratios of the earth mass and the moon mass, respectively,
              to the total earth-moon mass
  W         - angular velocity of moon around the earth (rad/s)
  x1,x2     - x-coordinates of the earth and of the moon, respectively,
              relative to the earth-moon barycenter (km)
  d0        - initial altitude of spacecraft (km)
  phi       - polar azimuth coordinate (degrees) of the spacecraft
              measured positive counterclockwise from the earth-moon line
  v0        - initial speed of spacecraft relative to rotating earth-moon
              system (km/s)
  gamma     - initial flight path angle (degrees)
  r0        - intial radial distance of spacecraft from the earth (km)
  x,y       - x and y coordinates of spacecraft in rotating earth-moon
              system (km)
  vx,vy     - x and y components of spacecraft velocity relative to
              rotating earth-moon system (km/s)
  f0        - column vector containing the initial valus of x, y, vx and vy
  t0,tf     - initial time and final times (s)
  t         - column vector of times at which the solution was computed
  f         - a matrix whose columns are:
              column 1: solution for x  at the times in t
              column 2: solution for y  at the times in t
              column 3: solution for vx at the times in t
              column 4: solution for vy at the times in t
  xf,yf     - x and y coordinates of spacecraft in rotating earth-moon
              system at tf
  vxf, vyf  - x and y components of spacecraft velocity relative to
              rotating earth-moon system at tf
  df        - distance from surface of the moon at tf
  vf        - relative speed at tf
  

  User M-functions required:  rkf45
  User subfunctions required: rates, circle
%}
% ---------------------------------------------

clear all; close all; clc

days   =  24*3600;
rmoon  =  1737;
rearth =  6378;
r12    =  384400;
m1     =  5974e21;
m2     =  7348e19;

M      =  m1 + m2;
pi_1   =  m1/M;
pi_2   =  m2/M;

mu1    =  398600;
mu2    =  4903.02;
mu     =  mu1 + mu2;

W      =  sqrt(mu/r12^3);
x1     = -pi_2*r12;
x2     =  pi_1*r12;

%...Input data:
d0     =  200;
phi    =  -90;
v0     =  10.9148;
gamma  =  20;
t0     =  0;
tf     =  3.16689*days;

r0     =  rearth + d0;
x      =  r0*cosd(phi) + x1;
y      =  r0*sind(phi);

vx     =  v0*(sind(gamma)*cosd(phi) - cosd(gamma)*sind(phi));
vy     =  v0*(sind(gamma)*sind(phi) + cosd(gamma)*cosd(phi));
f0     =  [x; y; vx; vy];

%...Compute the trajectory:
[t,f]  = rkf45(@rates, [t0 tf], f0);
x      = f(:,1);
y      = f(:,2);
vx     = f(:,3);
vy     = f(:,4);

xf     = x(end);
yf     = y(end);

vxf    = vx(end);
vyf    = vy(end);

df     = norm([xf - x2, yf - 0]) - rmoon;
vf     = norm([vxf, vyf]);

%...Output the results:
output
return

% ~~~~~~~~~~~~~~~~~~~~~~~~
function dfdt = rates(t,f)
% ~~~~~~~~~~~~~~~~~~~~~~~~   
%{
  This subfunction calculates the components of the relative acceleration
  for the restricted 3-body problem, using Equations 2.192a and 2.192b

  ax,ay - x and y components of relative acceleration (km/s^2)
  r1    - spacecraft distance from the earth (km)
  r2    - spacecraft  distance from the moon (km)
  f     - column vector containing x,  y,  vx and vy at time t
  dfdt  - column vector containing vx, vy, ax and ay at time t

  All other variables are defined above.

  User M-functions required: none
%}
% ------------------------    
x      = f(1);
y      = f(2);
vx     = f(3);
vy     = f(4);

r1     = norm([x + pi_2*r12, y]);
r2     = norm([x - pi_1*r12, y]);

ax     =  2*W*vy + W^2*x - mu1*(x - x1)/r1^3 - mu2*(x - x2)/r2^3;
ay     = -2*W*vx + W^2*y - (mu1/r1^3 + mu2/r2^3)*y;

dfdt   = [vx; vy; ax; ay];
end %rates

% ~~~~~~~~~~~~~
function output
% ~~~~~~~~~~~~~
%{
  This subfunction echos the input data and prints the results to the
  command window. It also plots the trajectory.

  User M-functions required: none
  User subfunction required: circle
%}
% -------------

fprintf('------------------------------------------------------------')
fprintf('\n Example 2.18: Lunar trajectory using the restricted')
fprintf('\n three body equations.\n')
fprintf('\n Initial Earth altitude (km)         = %g', d0)
fprintf('\n Initial angle between radial')
fprintf('\n   and earth-moon line (degrees)     = %g', phi)
fprintf('\n Initial flight path angle (degrees) = %g', gamma)
fprintf('\n Flight time (days)                  = %g', tf/days)
fprintf('\n Final distance from the moon (km)   = %g', df)
fprintf('\n Final relative speed (km/s)         = %g', vf)
fprintf('\n------------------------------------------------------------\n')

%...Plot the trajectory and place filled circles representing the earth
%   and moon on the the plot:
plot(x, y)
%   Set plot display parameters
xmin = -20.e3;  xmax =  4.e5;
ymin = -20.e3;  ymax =  1.e5;
axis([xmin xmax ymin ymax])
axis equal
xlabel('x, km'); ylabel('y, km')
grid on
hold on

%...Plot the earth (blue) and moon (green) to scale
earth  = circle(x1, 0, rearth);
moon   = circle(x2, 0, rmoon);
fill(earth(:,1), earth(:,2),'b')
fill( moon(:,1),  moon(:,2),'g')

% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function xy = circle(xc, yc, radius)
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%{
  This subfunction calculates the coordinates of points spaced
  0.1 degree apart around the circumference of a circle

  x,y    - x and y coordinates of a point on the circumference
  xc,yc  - x and y coordinates of the center of the circle
  radius - radius of the circle
  xy     - an array containing the x coordinates in column 1 and the 
           y coordinates in column 2

  User M-functions required: none
%}
% ----------------------------------
x      = xc + radius*cosd(0:0.1:360);
y      = yc + radius*sind(0:0.1:360);
xy     = [x', y'];

end %circle

end %output

end %Example_2_18
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~