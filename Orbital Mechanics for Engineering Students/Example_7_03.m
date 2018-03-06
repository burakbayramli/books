% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function Example_7_03
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%{
  This function plots the motion of chaser B relative to target A
  for the data in Example 7.3.

  mu       - gravitational parameter (km^3/s^2)
  RE       - radius of the earth (km)

           Target orbit at time t = 0:
  rp       - perigee radius (km)
  e        - eccentricity
  i        - inclination (rad)
  RA       - right ascension of the ascending node (rad)
  omega    - argument of perigee (rad)
  theta    - true anomaly (rad)
  ra       - apogee radius (km)
  h        - angular momentum (km^2/s)
  a        - semimajor axis (km)
  T        - period (s)
  n        - mean motion (rad/s)

  dr0, dv0 - initial relative position (km) and relative velocity (km/s)
             of B in the co-moving frame
  t0, tf   - initial and final times (s) for the numerical integration
  R0, V0   - initial position (km) and velocity (km/s) of A in the 
             geocentric equatorial frame
  y0       - column vector containing r0, v0
%}
% User M-functions required:  sv_from_coe, rkf45
% User subfunctions required: rates
% ---------------------------------------------

clear all; close all; clc

global mu

mu  = 398600;
RE  = 6378;

%...Input data:
%   Prescribed initial orbital parameters of target A:
rp    = RE + 300;
e     = 0.1;
i     = 0;
RA    = 0;
omega = 0;
theta = 0;

%   Additional computed parameters:
ra = rp*(1 + e)/(1 - e);
h  = sqrt(2*mu*rp*ra/(ra + rp));
a  = (rp + ra)/2;
T  = 2*pi/sqrt(mu)*a^1.5;
n  = 2*pi/T;

%   Prescribed initial state vector of chaser B in the co-moving frame: 
dr0 = [-1  0  0];
dv0 = [ 0 -2*n*dr0(1) 0];
t0  = 0;
tf  = 5*T;
%...End input data

%...Calculate the target's initial state vector using Algorithm 4.5:
[R0,V0] = sv_from_coe([h e RA i omega theta],mu);

%...Initial state vector of B's orbit relative to A
y0 = [dr0 dv0]';

%...Integrate Equations 7.34 using Algorithm 1.3:
[t,y] = rkf45(@rates, [t0 tf], y0);

plotit

return

% ~~~~~~~~~~~~~~~~~~~~~~~~
function dydt = rates(t,f)
% ~~~~~~~~~~~~~~~~~~~~~~~~
%{
  This function computes the components of f(t,y) in Equation 7.36.
  
  t             - time
  f             - column vector containing the relative position and
                  velocity vectors of B at time t
  R, V          - updated state vector of A at time t
  X, Y, Z       - components of R
  VX, VY, VZ    - components of V
  R_            - magnitude of R
  RdotV         - dot product of R and V
  h             - magnitude of the specific angular momentum of A

  dx , dy , dz  - components of the relative position vector of B
  dvx, dvy, dvz - components of the relative velocity vector of B
  dax, day, daz - components of the relative acceleration vector of B
  dydt          - column vector containing the relative velocity 
                  and acceleration components of B at time t

  User M-function required: rv_from_r0v0
%}
% ------------------------
%...Update the state vector of the target orbit using Algorithm 3.4:
[R,V] = rv_from_r0v0(R0, V0, t);

X  = R(1); Y  = R(2); Z  = R(3);
VX = V(1); VY = V(2); VZ = V(3);

R_    = norm([X Y Z]);
RdotV = dot([X Y Z], [VX VY VZ]);
h     = norm(cross([X Y Z], [VX VY VZ]));

dx    = f(1); dy  = f(2); dz  = f(3);
dvx   = f(4); dvy = f(5); dvz = f(6);

dax   =  (2*mu/R_^3 + h^2/R_^4)*dx - 2*RdotV/R_^4*h*dy + 2*h/R_^2*dvy;
day   =   -(mu/R_^3 - h^2/R_^4)*dy + 2*RdotV/R_^4*h*dx - 2*h/R_^2*dvx;
daz   = -mu/R_^3*dz;
    
dydt  = [dvx dvy dvz dax day daz]';    
end %rates

% ~~~~~~~~~~~~~
function plotit
% ~~~~~~~~~~~~~
%...Plot the trajectory of B relative to A:
% -------------
hold on
plot(y(:,2), y(:,1))
axis on
axis equal
axis ([0 40 -5 5])
xlabel('y (km)')
ylabel('x (km)')
grid on
box on
%...Label the start of B's trajectory relative to A:
text(y(1,2), y(1,1), 'o')
end %plotit    

end %Example_7_03
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~