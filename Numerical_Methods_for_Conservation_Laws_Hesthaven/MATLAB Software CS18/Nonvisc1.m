function [nu] = Nonvisc1(x,u,iV,m,N,h,nu0,kappa);
% function [nu] = Nonvisc1(x,u,iV,m,N,h,nu0,kappa);
% Purpose: Compute nonlinear viscosity following Persson and Peraire (2006)
nu = zeros(m+1,N); S = zeros(1,N); nuh = zeros(1,N); onev = ones(m+1,1);

% Extract coefficients and compute smoothness measure
uh = iV*u; S = uh(m+1,:).^2./sum(uh.*uh); se = log(S); 

% Compute elementwise viscosity
s0 = log(1/m^4);
nu1 = ((s0-kappa)<=se).*(se<=(s0+kappa)); nu2 = (se>(s0+kappa)); 
nuh = nu0*h/m*(nu1/2.*(1+sin(pi*(se-s0)/(2*kappa))) + nu2);

% Compute continuous viscosity
nue = zeros(1,N+2); nue = [nuh(1) nuh nuh(N)];
maxL = max(nue(1:N),nue(2:N+1)); maxR = max(nue(2:N+1),nue(3:N+2));
nu = onev*maxL + (x-onev*x(1,:))/h.*(onev*(maxR-maxL));
return