% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% Example_7_02
% ~~~~~~~~~~~~
%{ 
  This program produces a 3D plot of the motion of spacecraft B
  relative to A in Example 7.1. See Figure 7.4.
 
  User M-functions required: rv_from_r0v0 (Algorithm 3.4) 
                             sv_from_coe  (Algorithm 4.5)
                             rva_relative (Algorithm 7.1)
%}                              
% ---------------------------------------------
 
clear all; close all; clc
 
global mu
 
%...Gravitational parameter and earth radius:
mu  = 398600;
RE  = 6378;
 
%...Conversion factor from degrees to radians:
deg = pi/180;
 
%...Input data:
%   Initial orbital parameters (angular momentum, eccentricity,
%   inclination, RAAN, argument of perigee and true anomaly).
%   Spacecraft A:
h_A     = 52059;
e_A     = 0.025724;
i_A     = 60*deg;
RAAN_A  = 40*deg;
omega_A = 30*deg;
theta_A = 40*deg;
 
%   Spacecraft B:
h_B     = 52362;
e_B     = 0.0072696;
i_B     = 50*deg;
RAAN_B  = 40*deg;
omega_B = 120*deg;
theta_B = 40*deg;
 
vdir = [1 1 1];
 
%...End input data
 
%...Compute the initial state vectors of A and B using Algorithm 4.5:
[rA0,vA0] = sv_from_coe([h_A e_A RAAN_A i_A omega_A theta_A],mu);
[rB0,vB0] = sv_from_coe([h_B e_B RAAN_B i_B omega_B theta_B],mu);
 
h0 = cross(rA0,vA0);
 
%...Period of A:
TA = 2*pi/mu^2*(h_A/sqrt(1 - e_A^2))^3;
 
%...Number of time steps per period of A's orbit:
n = 100;
 
%...Time step as a fraction of A's period:
dt = TA/n;
 
%...Number of periods of A's orbit for which the trajectory
%   will be plotted:
n_Periods = 60;
 
%...Initialize the time:
t = - dt;
 
%...Generate the trajectory of B relative to A:
for count = 1:n_Periods*n
 
%...Update the time:
    t = t + dt;
    
%...Update the state vector of both orbits using Algorithm 3.4:
    [rA,vA] = rv_from_r0v0(rA0, vA0, t);
    [rB,vB] = rv_from_r0v0(rB0, vB0, t);
    
%...Compute r_rel using Algorithm 7.1:
    [r_rel, v_rel, a_rel] = rva_relative(rA,vA,rB,vB);
 
%...Store the components of the relative position vector
%   at this time step in the vectors x, y and z, respectively:
    x(count) = r_rel(1);
    y(count) = r_rel(2);
    z(count) = r_rel(3);
    r(count) = norm(r_rel);
    T(count) = t;
end
 
%...Plot the trajectory of B relative to A:
figure(1)
plot3(x, y, z)
hold on
axis equal
axis on
grid on
box off
view(vdir)
%   Draw the co-moving x, y and z axes:
line([0 4000],    [0 0],    [0 0]); text(4000,    0,    0, 'x')
line(   [0 0], [0 7000],    [0 0]); text(   0, 7000,    0, 'y')
line(   [0 0],    [0 0], [0 4000]); text(   0,    0, 4000, 'z')
 
%   Label the origin of the moving frame attached to A:
text (0, 0, 0, 'A')
 
%   Label the start of B's relative trajectory:
text(x(1), y(1), z(1), 'B')
 
%   Draw the initial position vector of B:
line([0 x(1)], [0 y(1)], [0 z(1)])
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
