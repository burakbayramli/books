% MATLAB file:  contour_sample.m
% Sample program to make (x,y) contour and pcolor plots for channel model.
% Students can use this as template for displaying mapped fields.
% This template defines channel width of 3000 km and length of 6000 km.
clear all
close all
Lx = 6000;                          % zonal wavelength in km
k = 2*pi/(Lx*1000);                 % zonal wavenumber in 1/m
m = pi/3.e6;		                % meridional wavenumber in 1/m
%  define the grid points on which fields are computed:
xx  = linspace(0,Lx,30);            % 30 gridpoints in x   
yy = linspace( -1500,1500,15);      % 15 gridpoints in y
[x,y] = meshgrid(xx*1000,yy*1000);  % Sets matrix for grid system in x and y

% Note the grid variables x, y are in meters  
% psi1 and psi2 are sample fields plotted using color fill and contouring
U = 10;                             % mean zonal wind m/s

% sample fields are stream function
psiM = real(4.0e6*exp(i*k*x).*cos(m*y)-U*(y-3.e6));
psiT = real(i*4.0e6*exp(i*k*x).*cos(m*y) );

figure(1)
axis square
omax = max(max(psiM));        % find maximum of psiM field
V = [0:omax/4:omax];          % specify  positive contour lines solid
V1 = [-omax:omax/4:0];        % specify negative contour lines dashed
omax = max(max(psiT));        % find maximum of psiT field
V2 = [0:omax/4:omax];         % specify  positive contour lines of psiT solid
V3 = [-omax:omax/4:0];        % specify negative contour lines of psiT dashed
subplot(2,1,1), contour(x/1000,y/1000,psiM,V,'k')
hold on
contour(x/1000,y/1000,psiM,V1,'k--')
pcolor(x/1000,y/1000,psiT), title('streamfunction (m^2/s)')
hold off
xlabel('x (km)'), ylabel('y (km)')
shading interp
colorbar('v')
subplot(2,1,2), contour(x/1000,y/1000,psiT,V2,'k')
hold on
contour(x/1000,y/1000,psiT,V3,'k--')
pcolor(x/1000,y/1000,psiM), title('streamfunction (m^2/s) ')
xlabel('x (km)'), ylabel('y (km)')
shading interp
colorbar('v')
hold off

