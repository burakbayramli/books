function psi = stream(Nx,Ny,Lx,Ly,zeta,x,y)
%  Function to invert vorticity for streamfunction
% Nx, Ny are number of grid points in x and y
% Lx and Ly are domain lengths in x and y
% zeta is the vorticity specified on the Nx by Ny grid
% specify domainLx = 2.*pi; Ly = 2.*pi; % domain size
% Fourier wavenumber operators
dx = [-Nx/2:Nx/2-1 ] * (2*pi/Lx);
dy = [-Ny/2:Ny/2-1 ] * (2*pi/Ly);
[DX DY] = meshgrid(dx,dy);
DX = fftshift(DX);   DY = fftshift(DY);
DZ = -(DX.^2+DY.^2);
DZi = DZ;  DZi(1,1) = 1;  DZi = 1./DZi;  DZi(1,1) = 0;
% now invert for streamfunction
zetaT = fft2(zeta); zetaT(1,1) = 0; % spectral vorticity
psiT  = DZi.*zetaT; % spectral streamfunction

psi = real(ifft2(psiT)); % grid point streamfunction
% check
check = real(ifft2(DZ.*fft2(psi))); % del^2(psi)
clc;
disp(['max error = ',num2str(max(max(abs(check-zeta))))])

