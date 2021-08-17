function [F,G,rho,u,v,p] = EulerFluxes2D(Q, gamma)
     
% function [F,G,rho,u,v,p] = EulerFluxes2D(Q, gamma)
% Purpose: evaluate primitive variables and Euler flux functions

% extract conserved variables
rho = Q(:,:,1); rhou = Q(:,:,2); rhov = Q(:,:,3); Ener = Q(:,:,4);

% compute primitive variables
u = rhou./rho; v = rhov./rho; p = (gamma-1)*(Ener - 0.5*(rhou.*u + rhov.*v));

% compute flux functions
F = zeros(size(Q)); 
F(:,:,1) = rhou; F(:,:,2) = rhou.*u + p; F(:,:,3) = rhov.*u; F(:,:,4) = u.*(Ener+p);

G = zeros(size(Q));
G(:,:,1) = rhov; G(:,:,2) = rhou.*v; G(:,:,3) = rhov.*v + p; G(:,:,4) = v.*(Ener+p);
return;
