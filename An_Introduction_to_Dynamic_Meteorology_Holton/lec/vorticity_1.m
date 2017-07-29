% MATLAB file vorticity_1.m   for use with problem M4.1
% Given a vorticity field script computes the corresponding streamfunction by
% inversion of the Laplacian using Fourier transforms
% Routine calls function "stream.m" to invert vorticity and
% then plots vorticity and streamfunction
% this version includes calculation of vorticity from the stream function as check
% using matlab function del2 and also plots velocity components with quiver plot
% Note that a doubly periodic domain is assumed in x and y but only
% 1/2 the y domain is plotted to mimic typical channel model with meridional
% velocity zero at northern and southern boundaries.
%%%%%%%%%%%%%%%%%%%%%%%%%%

% first define the grid points on which fields are computed:
%  (distances are in units of km).
clear all
close all
Lx = 6350;  Ly = 6350;        % channel dimensions in km
Nx = 128; Ny = 128;           % number of grid points in each direction
xx=linspace(-Lx/2,Lx/2,Nx);   % Nx gridpoints in x   
yy=linspace( -Ly/2,Ly/2,Ny);      % Ny gridpoints in y
[x,y]=meshgrid(xx,yy);        % Sets matrix for grid system in x and y
%   *********Define the function to be contoured*********
k = 2*pi/6000 ;               % zonal wavenumber in units of 1/ km
Const = 2.e7  ;               % constant value of streamfunction
%
%%%% Replace the expression for vorticity  zeta  below%%%%
% example given here is for  single sinusoids in x and y
zeta = 2.e-5*cos( k*x).*cos(k*y)+1.e-5*sin(k*y);
% Call to function to compute stream function from input vorticity
% NOTE that x and y are in km and k is in km-1 for convenience in graphing
% The factors of 1.e3 in call to stream.m are to convert to meters
psi = stream(Nx,Ny,Lx*1.e3,Ly*1.e3,zeta,x*1.e3,y*1.e3);
psi = psi+Const;                % add constant value to streamfunction
% vorticity is contoured 
figure (1) 
subplot(2,1,1)
hold on
[cs,h]=contour(x,y,zeta*1.e5,[0, .5, 1, 1.5, 2],'k');
axis([ -Lx/2 Lx/2 -Ly/4  Ly/4 ])
clabel(cs,h)
[cs,h]=contour(x,y,zeta*1.e5,[-.5, -1, -1.5, -2],'k--');
clabel(cs,h)
title('vorticity (s^-^1 times 1.e5)'), xlabel('x (km)'), ylabel('y (km)')
% streamfunction is contoured
subplot(2,1,2)
[cs,h]=contour(x,y,psi*1.e-7);
axis([-Lx/2 Lx/2 -Ly/4  Ly/4 ])
clabel(cs,h), title('streamfunction ( m^2/s times 1.e-7)')
xlabel('x (km)'), ylabel('y (km)')
hold off
figure(2)
% Check that finite difference Laplacian of psi gives back vorticity
% Use the matlab function del2 to evaluate Laplacian numerically
zetad=zeros(size(zeta));
zetad=4*del2(psi,1e3*Lx/Nx,1e3*Ly/Ny);

subplot(2,1,1)
hold on
[cs,h]=contour(x,y,zetad*1.e5,[0, .5, 1, 1.5, 2],'k');
axis([ -Lx/2 Lx/2 -Ly/4  Ly/4 ])
clabel(cs,h)
[cs,h]=contour(x,y,zetad*1.e5,[-.5, -1, -1.5, -2],'k--');
axis([ -Lx/2 Lx/2 -Ly/4  Ly/4 ])
clabel(cs,h)
title('vorticity (s^-^1 times 1.E5)')
xlabel('x (km)'), ylabel('y (km)')
subplot(2,1,2)
[px,py] = gradient(psi);    %computes the gradient of psi by finite differences
% Reduces number of grid points for plotting velocity vectors
for i=1:Nx/4
    for j=1:Ny/4
        xd(i,j)=x(4*i,4*j);
        yd(i,j)=y(4*i,4*j);
        pxd(i,j)=px(4*i,4*j);
        pyd(i,j)=py(4*i,4*j);
    end
end   
quiver(xd,yd,-pyd,pxd);
axis([-Lx/2 Lx/2 -Ly/4  Ly/4  ]);
title('velocity field')
xlabel('x (km)'),ylabel('y (km)');




