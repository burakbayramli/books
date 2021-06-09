% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% Example_2_13
% ~~~~~~~~~~~~
%{
  This program computes the state vector [R,V] from the initial
  state vector [R0,V0] and the change in true anomaly, using the
  data in Example 2.13

  mu - gravitational parameter (km^3/s^2)
  R0 - the initial position vector (km)
  V0 - the initial velocity vector (km/s)
  r0 - magnitude of R0
  v0 - magnitude of V0
  R  - final position vector (km)
  V  - final velocity vector (km/s)
  r  - magnitude of R
  v  - magnitude of V
  dt - change in true anomaly (degrees)

 User M-functions required: rv_from_r0v0_ta

%}
% --------------------------------------------------

clear all; clc
mu = 398600;

%...Input data:
R0 = [8182.4 -6865.9 0];
V0 = [0.47572 8.8116 0];
dt = 120;
%...End input data

%...Algorithm 2.3:
[R,V] = rv_from_r0v0_ta(R0, V0, dt, mu);

r  = norm(R);
v  = norm(V);
r0 = norm(R0);
v0 = norm(V0);

fprintf('-----------------------------------------------------------')
fprintf('\n Example 2.9 \n')
fprintf('\n Initial state vector:\n')
fprintf('\n   r = [%g, %g, %g] (km)', R0(1), R0(2), R0(3))
fprintf('\n     magnitude = %g\n', norm(R0))

fprintf('\n   v = [%g, %g, %g] (km/s)', V0(1), V0(2), V0(3))
fprintf('\n     magnitude = %g', norm(V0))

fprintf('\n\n State vector after %g degree change in true anomaly:\n', dt)
fprintf('\n   r = [%g, %g, %g] (km)', R(1), R(2), R(3))
fprintf('\n     magnitude = %g\n', norm(R))
fprintf('\n   v = [%g, %g, %g] (km/s)', V(1), V(2), V(3))
fprintf('\n     magnitude = %g', norm(V))
fprintf('\n-----------------------------------------------------------\n')
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
