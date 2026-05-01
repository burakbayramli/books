% fftpoi - Program to solve the Poisson equation using 
% MFT method (periodic boundary conditions)
clear all; help fftpoi;  % Clear memory and print header

%* Initialize parameters (system size, grid spacing, etc.)
eps0 = 8.8542e-12;   % Permittivity (C^2/(N m^2))
N = 50;   % Number of grid points on a side (square grid)
L = 1;    % System size
h = L/N;  % Grid spacing for periodic boundary conditions
x = ((1:N)-1/2)*h;  % Coordinates of grid points
y = x;              % Square grid
fprintf('System is a square of length %g \n',L);

%* Set up charge density rho(i,j) 
rho = zeros(N,N);  % Initialize charge density to zero
M = input('Enter number of line charges: ');
for i=1:M
  fprintf('\n For charge #%g \n',i);
  r = input('Enter position [x y]: ');
  ii=round(r(1)/h + 1/2);   % Place charge at nearest
  jj=round(r(2)/h + 1/2);   % grid point
  q = input('Enter charge density: ');
  rho(ii,jj) = rho(ii,jj) + q/h^2;
end

%* Compute matrix P
cx = cos((2*pi/N)*(0:N-1));
cy = cx;
numerator = -h^2/(2*eps0);
tinyNumber = 1e-20;  % Avoids division by zero
for i=1:N
 for j=1:N
   P(i,j) = numerator/(cx(i)+cy(j)-2+tinyNumber);
 end
end

%* Compute potential using MFT method
rhoT = fft2(rho);   % Transform rho into wavenumber domain
phiT = rhoT .* P;   % Computing phi in the wavenumber domain
phi = ifft2(phiT);  % Inv. transf. phi into the coord. domain
phi = real(phi);    % Clean up imaginary part due to round-off

%* Compute electric field as E = - grad phi
[Ex Ey] = gradient(flipud(rot90(phi))); 
magnitude = sqrt(Ex.^2 + Ey.^2);         
Ex = -Ex ./ magnitude;     % Normalize components so
Ey = -Ey ./ magnitude;     % vectors have equal length

%* Plot potential and electric field
figure(1); clf;
contour3(x,y,flipud(rot90(phi,1)),35);
xlabel('x'); ylabel('y'); zlabel('\Phi(x,y)');
figure(2); clf;
quiver(x,y,Ex,Ey)        % Plot E field with vectors
title('E field (Direction)'); xlabel('x'); ylabel('y');
axis('square');  axis([0 L 0 L]);
   
