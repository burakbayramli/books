function psi = stream1(Nxl,Nyl,Lx,dy,zeta)
% Invert vorticity for streamfunction.
% Nx is number of grid points in x, Ny number in y.
% Lx and Ly are domain lengths in x and y
% zeta is the vorticity specified on the Nx by Ny grid.
% Use cyclic conditions in x with FFT, Finite Difference in y.
psiT=zeros(size(zeta));
psi=psiT;
k = [0:Nxl-1 ] * (2*pi/(Lx*1000));  % Fourier wavenumber operators
K2 = k.^2;
dy2 = dy^2;
K2dy2 = K2*dy2;
% take fft of vorticity in x direction
fzeta = fft(zeta,Nxl,2);
% now invert for streamfunction
for s = 1:Nxl/2;
    %   Coefficients for tridiagonal solver
    A = -(K2dy2(s) +2);
    B(2:Nyl) = dy2*fzeta(2:Nyl,s);      % forcing term
    B(2) = B(2);                        % boundary condition for y = 0
    e = ones(Nyl,1);
    % Define the tridiagonal matrix for finite difference equation
    M = spdiags([e A*e e], -1:1, Nyl-1,Nyl-1);
    % solve M*psiT = B for psiT by matrix inversion
    psiT(2:Nyl,s) = M\B(2:Nyl).';
    psiT(1,s) = 0;
end
psi(:,1:Nxl) = 2*real(ifft(psiT,Nxl,2)); % grid point streamfunction
psi(:,Nxl+1) = psi(:,1);
