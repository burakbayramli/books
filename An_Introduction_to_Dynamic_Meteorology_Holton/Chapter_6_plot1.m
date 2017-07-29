% MATLAB file: Chapter_6_plot.m    template for problems M6.1, 6.2, 6.3
% Sample quiver and contour plotting for Chapter 6.
% First define the grid points on which fields are computed.
% (Distances are in units of km).
clear all                       % clear the graphics window
close all                       % clear the workspace
xx=linspace(-3000,3000,30);     % 30 gridpoints in x   
yy=linspace( -1000,1000,10);    % 10 gridpoints in y
[x,y]=meshgrid(xx,yy);          % Sets matrix for grid system in x and y
%   *********Define the vectors to be plotted*********
k = 2*pi/6000;                  % zonal wavenumber in units of 1/ km
l = 2*pi/4000;			        % meridional wavenumber in 1/km
V = 25;                         % wave amplitude speed m/s
U = 30;                         % zonal mean wind
c = 25;                         % phase speed
cor = 1.e-4;                    % Coriolis parameter
beta= 1.67e-11;                 % df/dy
% NOTE that x and y are in km and k is in km-1 for convenience in 
%graphing the factors of 1000 are to convert to meters
%specified geopotential
phi = 55000-U*cor*(y)*1.e3+ V*cor/k*1.e+3*sin(k*x).*cos(l*(y));
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Dummy fields used to setup graphics
% Students to insert correct formulas for the fields below
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ug = U*ones(size(x));
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The following fields are given dummy values and must be calculated
% by the student
vg =  zeros(size(x));           % meridional wind
zeta = phi;                     %  vorticity 
advzeta = phi;                  % relative vorticity advection
advbeta = phi;                  % planetary vorticity advection
dzetadt = phi;                  % vorticity tendency
div=phi;                        % divergence

uad = ug;           % divergent part of zonal ageostrophic wind
vad = vg;           % divergent part of ageostrophic wind    
divag= phi;         % diveegence of ageostrophic wind
vortag = phi;       % vorticity of ageostrophic wind
uan = ug;           % nondivergent ageostrophic wind
van = vg;           % nondivergent ageostrophic wind
% phi/g is contoured with contour lines labelled
% figure 1 is template for contour plots on isobaric surfaces
figure(1)
subplot(2,1,1)
[cs,h]=contour(x,y,phi/9.8);
clabel(cs,h),title('geopotential height')
xlabel('x (km)'), ylabel('y (km)')
subplot(2,1,2)
[cs,h]=contour(x,y,zeta);
clabel(cs,h)
hold on
quiver(x,y,ug,vg),title('geostrophic wind and vorticity')
xlabel('x (km)'), ylabel('y (km)')
figure(2) 
subplot(2,1,1)
[cs,h]=contour(x,y,advzeta);
clabel(cs,h),title('relative vorticity advection')
xlabel('x (km)'), ylabel('y (km)')
subplot(2,1,2)
[cs,h]=contour(x,y,advbeta);
clabel(cs,h), title('planetary vorticity advection')
xlabel('x (km)'), ylabel('y (km)');
axis([-3000 3000 -1000 1000])
figure(3)
subplot(2,1,1)
[cs,h] = contour(x,y,dzetadt);
clabel(cs,h), title('vorticity tendency')
xlabel('x (km)'), ylabel('y (km)')
subplot(2,1,2)
[cs,h] = contour(x,y,div);
clabel(cs,h)
hold on
quiver(x,y,uad,vad)
title('divergent ageostrophic wind and divergence')
xlabel('x (km)'), ylabel('y (km)');
axis([-3000 3000 -1000 1000])
figure(4)
subplot(2,1,1)
[cs,h] = contour(x,y,divag);
clabel(cs,h), title('divergence of ageostrophic wind')
xlabel('x (km)'), ylabel('y (km)')
subplot(2,1,2)
[cs,h] = contour(x,y,vortag);
clabel(cs,h)
xlabel('x (km)'), ylabel('y (km)')
hold on
quiver(x,y,uan,van)
title('nondivergent ageostrophic wind and its vorticity')
xlabel('x (km)'), ylabel('y (km)')





