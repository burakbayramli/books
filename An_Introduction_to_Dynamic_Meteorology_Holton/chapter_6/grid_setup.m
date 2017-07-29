%
% set up the grid for numerical solution of QG diagnostic equations
% *** EVERYTHING HERE IS NON-DIMENSIONAL ***
% *** DOMAIN IS PERIODIC IN X AND Y ***
%
% Originator: G.J. Hakim, University of Washington
%
% released under GNU General Public License version 3. http://www.gnu.org/licenses/gpl.html
%
% version control:
% $Date: 2012-02-09 09:31:05 -0800 (Thu, 09 Feb 2012) $
% $Revision: 788 $
% $Author: hakim $
% $Id: grid_setup.m 788 2012-02-09 17:31:05Z hakim $

% grid size and spectral discretization
kmax = 32/2; % number of x waves (=0.5 number of grid points)
lmax = 32/2; % number of y waves (=0.5 number of grid points)
pmax = 50;   % number of vertical levels (staggered levels)

XL = 5.0;    % x domain length
YL = 5.0;    % y domain length
ZH = 1.0;    % z domain length

% Fourier factors
facx=2.*pi/XL; facy=2.*pi/YL;

% shortcut variables
nlevs = pmax; Nx = 2*kmax; Ny = 2*lmax; Nz = pmax;

% coordinates (x,y)
ddx = XL/Nx; ddy = YL/Ny; % horizontal grid spacing
xx = 0:ddx:XL;   x = xx(2:Nx+1) - XL/2;
yy = 0:ddy:YL;   y = yy(2:Ny+1) - YL/2;
[xg, yg] = meshgrid(x,y);

% Fourier wavenumber operators
dx = [-Nx/2:Nx/2-1] * facx;
dy = [-Ny/2:Ny/2-1] * facy;
[DX DY] = meshgrid(dx,dy);
DX = fftshift(DX); DY = fftshift(DY);

% vertical grid
dz = ZH/real(nlevs); 
% this is the computation grid for phi, staggered one-half level from boundaries
z = [1:1:pmax]*dz - (dz/2);
% this is the grid for w & theta, unstaggered and used for plotting
zu = [0:1:pmax]*dz;
imid = kmax;
jmid = lmax;
kmid = fix(pmax/2);
