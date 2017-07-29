% MATLAB script:  coriolis.m  (uses functions xprim1.m and xprim2.m)
% Problems M1.1, M1.2, andM1.3
% Script to compute constant angular momentum trajectories in spherical 
% coordinates with curvature terms included [eqs. (1.10a) and (1.11a)]
% (shown in Figure 1, and with curvature terms omitted [eqs. (1.12a)
% and (1.12b)] (shown in Figure 2).
% black diamond on plot marks initial position of particle
close all
disp('Initial longitude is zero. Specify latitude and speed when asked.')
init_lat = input('Specify an initial latitude in degrees  ');
u0 = input('Specify a zonal wind in m/s ' );
v0 = input('Specify a meridional wind in m/s ');
rad= 6.37e6;
omega=7.292e-5;
lat0 = pi*init_lat/180.;
runtime = input('Specify integration time in days  ');
time = runtime*24.*3600.;
options = odeset('RelTol', 1.e-6);
[t,x] = ode45('xprim1',[0 time],[u0 v0 0 lat0],options,rad,omega);
long = x(:,3)*180/pi;      %converts longitude to degrees
lat = x(:,4)*180./pi;       %converts latitude to degrees
A1 = -1+min(long); A2 =1+max(long);
A3 =  -1+min(lat); A4 = 1+max(lat);
figure(1)
plot(long,lat)
axis([A1 A2 A3 A4])
xlabel ('longitude')
ylabel ('latitude')
title('Trajectory with curvature terms')
hold on
plot(long(1),lat(1),'kd')
[t,x] = ode45('xprim2',[0 time],[u0 v0 0 lat0],options,rad,omega);
long = x(:,3)*180/pi;           % converts longitude to degrees
lat = x(:,4)*180./pi;           % converts latitude to degrees
A1 = -1+min(long); A2 =1+max(long);
A3 =  -1+min(lat); A4 = 1+max(lat);
figure(2)
plot(long,lat,'r')
axis([A1 A2 A3 A4])
xlabel ('longitude')
ylabel ('latitude')
title('Trajectory without curvature terms')
hold on
plot(long(1),lat(1),'kd')



