% MATLAB file: Chapter_6_plot2.m (revised 6/5/02).
% Sets up graphics for longitude-height plots using subplotting  
% First define the grid points on which fields are computed:
% (Distances are in units of km).
xx=linspace(-3000,3000,30);  % 30 gridpoints in x   
pp=linspace(0,1000,11);      % 10 gridpoints in pressure in hPa
[x,p]=meshgrid(xx,pp);       % Sets matrix for grid system in x and p
close all                    % close the graphics window  
%   *********Define the function to be contoured*********
m = pi/1000;                 % vertical pressure wavenumber in 1/hPa
k = 2*pi/6.e6;              % zonal wavenumber in units of 1/m
c = 20;                    % zonal phase speed m/s
cor = 1.e-4;                  % Coriolis parameter (s-1)
% Sample vorticity height section at t=0.
% NOTE that x is in km  for convenience in graphing
% the factors of 1000 are to convert k to 1/km for arguments
% in sinusoidal functions
%
t = 0;                        % time in seconds
% Sample field for contour plot of vorticity
zeta = -c*k*sin(k*(x*1.e3-c*t)).*(1+cos(m*p)); 

% Student must replace next three statements by  formulas for
% each field to be computed  (e. g., vorticity advection) 
% Note that fields are multiplied by a factor to give a nice number
% Choose a factor so that contoured values are order unity and show 
% the multiplication factor in the title.
% These are dummy fields:
zetaprim = zeta;
zadvect = zeta;
div = zeta;
%
figure(1)
subplot(3,1,1)
[cs,h]=contour(x,p,zetaprim*1.e5);
clabel(cs,h), title('vorticity tendency times 1.e5')
xlabel('x (km)'), ylabel('pressure (hPa)')      
axis ij                     % corrects pressure labelling
subplot(3,1,2)
[cs,h]=contour(x,p,zadvect*1.e5);
clabel(cs,h), title('vorticity advection times 1.e5')
xlabel('x (km)'), ylabel('pressure (hPa)')     
axis ij                     % corrects pressure labelling
subplot(3,1,3) 
[cs,h]=contour(x,p,div*1.e5);
axis ij                     % corrects pressure labelling
clabel(cs,h), title('divergence times 1.e5')

xlabel('x (km)'), ylabel('pressure (hPa)')






