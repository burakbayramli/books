% MATLAB file: localized_vorticity.m  for use with Chapter 6.
% Shows effect of 2 or 3 waves on vorticity pattern vs streamfunction
% Sample contour plot routine for scalars to be used with problems
% in Chapter 6 of Introduction to Dynamic Meteorology.
% First define the grid points on which fields are computed:
% (Distances are in units of km).
clear all
close all
xx=linspace(-3000,3000,60);     % 150 gridpoints in x   
yy=linspace( 0,1500,30);        % 50 gridpoints in y
[x,y]=meshgrid(xx,yy);          % Sets matrix for grid system in x and y
%   *********Define the function to be contoured*********
k = 4*pi/6000;                  % zonal wavenumber in units of 1/ km
c = 25;                         % wave wind speed m/s
f = 1.e-4;                      % Coriolis parameter (s-1)
U = 20;                         % zonal velocity
% geopotential at 500 hPa  in meters
% NOTE that x and y are in km and k is in km-1 for convenience in graphing
% the factors of 1000 are to convert the expression to geopotential meters
%
z1 = 55000.  - (U*f*1.e3).*y -(c*f*1000)/k.*(cos(k.*x)+.5*cos(2*k*x)+...
    .1*cos(3*k*x)).*sin(1*k*y);
zeta = + k^2*1.e-6*(c*1000)/k.*((1+1)*cos(k.*x)+.6*(4+1)*cos(2*k*x)...
    +.1*(9+1)*cos(3*k*x)).* sin(1*k*y);
%
% geopotential is contoured 
subplot(2,1,1)
hold on
cs=contour(x,y,z1/100);
clabel(cs), title('geopotential height in dm')
xlabel('x (km)'), ylabel('y (km)')
% vorticity is contoured
subplot(2,1,2)
cs=contour(x,y,zeta*1.e5,4);
clabel(cs), title('relative vorticity \times 10^5')
xlabel('x (km)'),ylabel('y (km)')
hold off










