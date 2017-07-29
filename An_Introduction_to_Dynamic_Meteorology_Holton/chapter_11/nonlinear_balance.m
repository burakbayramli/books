% MMATLAB file:  nonlinear_balance.m   for use with problem M11.2
% Given a streamfunction script computes the corresponding geopotential
% From eq. (11.15) for simple sinusoidal wave 
% Inversion of the Laplacian employs Fourier transform
% Routine calls function "invert_labplace.m" to invert Laplacian and
% then plots geopotential and streamfunction.
% Also plots velocity components with quiver plot.
% Note that a  periodic domain is assumed in x and y but only
% the y domain has meridional velocity zero at equator.
%%%%%%%%%%%%%%%%%%%%%%%%%%

% First define the grid points on which fields are computed:
% (Distances are in units of km).
clear all
close all
Lx = 6.e6;  Ly = 6.e6;          % channel dimensions in  m
Nx = 64; Ny = 64 ;              % number of grid points in each direction
xx = linspace(-Lx/2,Lx/2,Nx);   % Nx gridpoints in x   
yy = linspace( -Ly/2,Ly/2,Ny);  % Ny gridpoints in y
[x,y] = meshgrid(xx,yy);        % Sets matrix for grid system in x and y in meters

%   *********Define the function to be contoured*********
k = 2*pi/Lx;                    % zonal wavenumber in units of 1/m
A = 4.e7;                       % constant value of wave streamfunction
a = 6.37e6;                     % radius of earth
beta = 0*2.29e-11*cos(y/a);     % beta  
cor=1.46e-4*sin(pi/6);
%
% specified streamfunction has mean zonal wind plus wave component
% example given here is for  single sinusoids in x and y

psia = A*sin(k*x).*(sin(k*y));
psib = A*cos(k*x)*cos(k*y);
psi = psia;
% compute right hand side term in eq. (11.15)using form given in problem 11.3
Gxy = 2*k^4*( psia.^2-psib.^2);
RHS = -2*A*k^2*cor.*sin(k*x).*sin(k*y)+beta.*A*k*sin(k*x).*cos(k*y)+Gxy;
phi = invert_laplacian(Nx,Ny,Lx,Ly,RHS,x,y);
% streamfunction is contoured  
figure (1) 
subplot(2,1,1)
[cs,h]=contour(x/1000,y/1000,psi*1.e-7,'k');
axis([ -Lx/2000 Lx/2000 0  Ly/2000 ])
hold on
clabel(cs,h)
[cs,h]=contour(x/1000,y/1000,psi*1.e-7,'k--');
clabel(cs,h)
title('streamfunction times 1.e-7')
xlabel('x  (km)'), ylabel('y  (km)')
subplot(2,1,2)
hold on
[cs,h]=contour(x/1000,y/1000,phi/9.8,'k');
axis([ -Lx/2000 Lx/2000 0  Ly/2000 ])
clabel(cs,h)
[cs,h]=contour(x/1000,y/1000,phi/9.8,'k--');
axis([ -Lx/2000 Lx/2000 0  Ly/2000 ])
clabel(cs,h)
title('geopotential height')
xlabel('x  (km)'), ylabel('y  (km)')
figure(2)
[cs,h]=contour(x/1000,y/1000,phi/9.8,'k');
axis([ -Lx/2000 Lx/2000 0  Ly/2000 ])
clabel(cs,h)
hold on
title('geopotential height (m)')
xlabel('x  (km)'), ylabel('y  (km)')
u = -A*k*sin(k*x).*cos(k*y);
v = A*k*cos(k*x).*sin(k*y);
% Reduces number of grid points for plotting velocity vectors
for i=1:Nx/4
    for j=1:Ny/4
        xd(i,j) = x(4*i,4*j);
        yd(i,j) = y(4*i,4*j);
        uxd(i,j) = u(4*i,4*j);
        vyd(i,j) = v(4*i,4*j);
    end
end   
quiver(xd/1000,yd/1000,uxd,vyd)
axis([-Lx/2000 Lx/2000 0  Ly/2000  ])
title(' geopotential and velocity field')
xlabel('x  (km)'), ylabel('y  (km)')




