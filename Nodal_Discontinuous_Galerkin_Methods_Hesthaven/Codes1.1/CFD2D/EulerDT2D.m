function dt = EulerDT2D(Q, gamma)

% function dt = EulerDT2D(Q, gamma)
% purpose: compute the time step dt for the compressible Euler equations

Globals2D;

rho = Q(:,:,1); rhou = Q(:,:,2); rhov = Q(:,:,3); Ener = Q(:,:,4);
rho = rho(vmapM); rhou = rhou(vmapM); rhov = rhov(vmapM); Ener = Ener(vmapM);

u = rhou./rho; v = rhov./rho;
p = (gamma-1.0)*(Ener - rho.*(u.^2+v.^2)/2); c = sqrt(abs(gamma*p./rho));

dt = 1/max( ((N+1)^2)*.5*Fscale(:).*(sqrt ( u(:).^2 + v(:).^2 ) + c(:)));

rhoprange = [min(min(rho)), max(max(rho)), min(min(p)), max(max(p))]
return
