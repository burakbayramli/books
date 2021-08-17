% Purpose: Matlab script to compute the advection terms for incompressible Navier-Stokes

% evaluate flux vectors
fxUx = Ux.^2;  fyUx = Ux.*Uy;  fxUy = Ux.*Uy; fyUy = Uy.^2;

% save old nonlinear terms
NUxold = NUx; NUyold = NUy; 

% evaluate inner-product of test function gradient and flux functions
NUx = Div2D(fxUx, fyUx); NUy = Div2D(fxUy, fyUy);

% interpolate velocity to face nodes on element faces
UxM = zeros(Nfp*Nfaces, K); UxP = zeros(Nfp*Nfaces, K); 
UyM = zeros(Nfp*Nfaces, K); UyP = zeros(Nfp*Nfaces, K); 
UxM(:) = Ux(vmapM); UyM(:) = Uy(vmapM); UxP(:) = Ux(vmapP); UyP(:) = Uy(vmapP);

% set '+' trace of velocity at boundary face nodes
UxP(mapI) = bcUx(mapI); UxP(mapW) = bcUx(mapW); UxP(mapC) = bcUx(mapC); 
UyP(mapI) = bcUy(mapI); UyP(mapW) = bcUy(mapW); UyP(mapC) = bcUy(mapC);

% evaluate flux vectors at '-' and '+' traces at face nodes
fxUxM = UxM.^2; fyUxM = UxM.*UyM; fxUyM = UxM.*UyM; fyUyM = UyM.^2;
fxUxP = UxP.^2; fyUxP = UxP.*UyP; fxUyP = UxP.*UyP; fyUyP = UyP.^2;

% evaluate dot product of normal and velocity at face nodes
UDotNM = UxM.*nx + UyM.*ny; UDotNP = UxP.*nx + UyP.*ny;
maxvel = max(abs(UDotNM), abs(UDotNP)) ;

% evaluate maximum normal velocity at face face nodes
maxvel = reshape(maxvel, Nfp, Nfaces*K);
maxvel = ones(Nfp, 1)*max(maxvel, [], 1); 
maxvel = reshape(maxvel, Nfp*Nfaces, K);

% form local Lax-Friedrichs/Rusonov fluxes
fluxUx = 0.5*( -nx.*(fxUxM-fxUxP) - ny.*(fyUxM-fyUxP)  - maxvel.*(UxP - UxM));
fluxUy = 0.5*( -nx.*(fxUyM-fxUyP) - ny.*(fyUyM-fyUyP)  - maxvel.*(UyP - UyM));

% put volume and surface terms together
NUx = NUx + LIFT*(Fscale.*fluxUx); NUy = NUy + LIFT*(Fscale.*fluxUy);

% compute (U~,V~)
UxT = ((a0*Ux+a1*Uxold) - dt*(b0*NUx + b1*NUxold))/g0; 
UyT = ((a0*Uy+a1*Uyold) - dt*(b0*NUy + b1*NUyold))/g0; 
