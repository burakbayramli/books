function flux = EulerRoe2D(nx, ny, QM, QP, gamma)
  
% function flux = EulerRoe2D(nx, ny, QM, QP, gamma)
% Purpose: compute surface fluxes for Euler's equations using an
%          approximate Riemann solver based on Roe averages

Nfields = 4;

% Rotate "-" trace momentum to face normal-tangent coordinates
rhouM = QM(:,:,2); rhovM = QM(:,:,3); EnerM = QM(:,:,4);
QM(:,:,2) =  nx.*rhouM + ny.*rhovM; QM(:,:,3) = -ny.*rhouM + nx.*rhovM;

% Rotate "+" trace momentum to face normal-tangent coordinates
rhouP = QP(:,:,2); rhovP = QP(:,:,3); EnerP = QP(:,:,4);
QP(:,:,2) = nx.*rhouP + ny.*rhovP; QP(:,:,3) =-ny.*rhouP + nx.*rhovP;

% Compute fluxes and primitive variables in rotated coordinates  
[fxQM,fyQM,rhoM,uM,vM,pM] = EulerFluxes2D(QM, gamma);
[fxQP,fyQP,rhoP,uP,vP,pP] = EulerFluxes2D(QP, gamma);

% Compute enthalpy
HM = (EnerM+pM)./rhoM; HP = (EnerP+pP)./rhoP;

% Compute Roe average variables
rhoMs = sqrt(rhoM); rhoPs = sqrt(rhoP);

rho = rhoMs.*rhoPs;
u   = (rhoMs.*uM + rhoPs.*uP)./(rhoMs + rhoPs);
v   = (rhoMs.*vM + rhoPs.*vP)./(rhoMs + rhoPs);
H   = (rhoMs.*HM + rhoPs.*HP)./(rhoMs + rhoPs);

c2  = (gamma-1)*(H - 0.5*(u.^2 + v.^2)); c = sqrt(c2);

% Riemann fluxes
dW1 = -0.5*rho.*(uP-uM)./c + 0.5*(pP-pM)./c2;  
dW2 = (rhoP-rhoM) - (pP-pM)./c2;
dW3 = rho.*(vP-vM);
dW4 = 0.5*rho .*(uP-uM)./c + 0.5*(pP-pM)./c2;

dW1 = abs(u-c).*dW1; dW2 = abs(u).*dW2; dW3 = abs(u).*dW3; dW4 = abs(u+c).*dW4;

% Form Roe fluxes
fx = (fxQP+fxQM)/2;

fx(:,:,1) =fx(:,:,1)-(dW1.*1       +dW2.*1            +dW3.*0+dW4.*1       )/2;
fx(:,:,2) =fx(:,:,2)-(dW1.*(u-c)   +dW2.*u            +dW3.*0+dW4.*(u+c)   )/2;
fx(:,:,3) =fx(:,:,3)-(dW1.*v       +dW2.*v            +dW3.*1+dW4.*v       )/2;
fx(:,:,4) =fx(:,:,4)-(dW1.*(H-u.*c)+dW2.*(u.^2+v.^2)/2+dW3.*v+dW4.*(H+u.*c))/2;

% rotate back to Cartesian
flux = fx;
flux(:,:,2) = nx.*fx(:,:,2) - ny.*fx(:,:,3);
flux(:,:,3) = ny.*fx(:,:,2) + nx.*fx(:,:,3);
return;
