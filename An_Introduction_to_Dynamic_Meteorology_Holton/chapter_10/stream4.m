function psi = stream1(Nyl,dy,k,lam2,zeta)
% Function to invert vorticity for streamfunction for single zonal
% wavenumber.
% Ny = Nyl+1 is number of grid points in y.
% Ly is domain width.
% zeta is the wave vorticity specified on the Ny grid.
% Use  finite Differences in y.
psi=zeros(size(zeta));
K2 = k^2+lam2;
dy2 = dy^2;
K2dy2 = K2*dy2;
% now invert for streamfunction
% Coefficients for tridiagonal solver
A = -(K2dy2 +2);
B(2:Nyl) = dy2*zeta(2:Nyl);   %forcing term
e = ones(Nyl,1);
% Define the tridiagonal matrix for finite difference equation
M = spdiags([e A*e e], -1:1, Nyl-1,Nyl-1);
% solve M*psiT = B for psiT by matrix inversion
psi(2:Nyl) = M\B(2:Nyl).';
