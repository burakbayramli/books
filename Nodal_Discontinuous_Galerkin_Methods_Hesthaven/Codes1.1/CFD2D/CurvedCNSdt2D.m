function dt = CurvedCNSdt2D(Q, gamma, mu)

% function dt = CurvedCNSdt2D(Q, gamma, mu)
% Purpose: compute stable time step size for compressible Navier-Stokes solver

Globals2D;

% extract field variables
rho = Q(:,:,1); rhou = Q(:,:,2); rhov = Q(:,:,3); Ener = Q(:,:,4);

% evaluate fields at surface nodes
rho = rho(vmapM); rhou = rhou(vmapM); rhov = rhov(vmapM); Ener = Ener(vmapM);

% compute primitive variables
u = rhou./rho; v = rhov./rho;
p = (gamma-1.0)*(Ener - rho.*(u.^2+v.^2)/2);
c = sqrt(abs(gamma*p./rho));

h = 2./Fscale(:);
lam = sqrt ( u(:).^2 + v(:).^2 ) + c(:);
dt = 0.5*min(1./( (N+1)^2*lam./h + (N+1)^4*mu./(h.^2)))
return