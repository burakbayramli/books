% MATLAB file: vorticity_demo.m       
% Demonstration to show streamfunction for point source of vorticity
% program uses fourier series in x and y for inversion
% specify domain size in km
clear all
close all
Lx = 2.*pi*1000; Ly = 2.*pi*1000; % domain size
Nx = 128; Ny = 128; % number of grid points in x and y
x=linspace(-Lx/2,Lx/2,129);
y=linspace(-Ly/2,Ly/2,129);
[xx,yy]=meshgrid(x(1:128),y(1:128));
% Fourier wavenumber operators
dx = [-Nx/2:Nx/2-1] * (2*pi/Lx);
dy = [-Ny/2:Ny/2-1] * (2*pi/Ly);
[DX DY] = meshgrid(dx,dy);
DX = fftshift(DX);   DY = fftshift(DY);
DZ = -(DX.^2+DY.^2);
DZi = DZ;  DZi(1,1) = 1;  DZi = 1./DZi;  DZi(1,1) = 0;

% specify vorticity
zeta = zeros(Nx,Ny); zeta(Nx/2,Ny/2) = 1.;  % discrete delta function
zeta = zeta - sum(sum(zeta))/(Nx*Ny);       % zero mean

% now invert for streamfunction
zetaT = fft2(zeta); zetaT(1,1) = 0;         % spectral vorticity
psiT  = DZi.*zetaT;                         % spectral streamfunction

psi = real(ifft2(psiT));                    % grid point streamfunction

% plot results
figure(1);clf
pcolor(xx,yy,zeta); shading interp; caxis([-1 1]); colorbar
hold on;
[cs,h]=contour(xx,yy,psi,'k');
clabel(cs,h)
xlabel('x in km'), ylabel('y in km')
title('vorticity (color), streamfunction (contours)')
% check
check = real(ifft2(DZ.*fft2(psi)));         % del^2(psi)
clc;disp(['max error = ',num2str(max(max(abs(check-zeta))))])