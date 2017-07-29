function phi = invert_laplacian(Nx,Ny,Lx,Ly,zeta,x,y);
% Inverts laplacian of geopotential
% Nx, Ny are number of grid points in x and y.
% Lx and Ly are domain lengths in x and y ( in meters)
% zeta is the vorticity specified on the Nx by Ny grid
% Specify domainLx = 2.*pi; Ly = 2.*pi; % domain size.

% Fourier wavenumber operators.
dx = [-Nx/2:Nx/2-1 ] * (2*pi/Lx);
dy = [-Ny/2:Ny/2-1 ] * (2*pi/Ly);
[DX DY] = meshgrid(dx,dy);
DX = fftshift(DX);   DY = fftshift(DY);
DZ = -(DX.^2+DY.^2);
DZi = DZ;  DZi(1,1) = 1;  DZi = 1./DZi;  DZi(1,1) = 0;
zetaT = fft2(zeta); zetaT(1,1) = 0;     % spectral laplacian
phiT = DZi.*zetaT;                     % spectral geopotential
phi = real(ifft2(phiT));                % grid point geopotential
  
