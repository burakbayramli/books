function flux = EulerRoe2D(nx, ny, QM, QP, gamma)
  
% function flux = EulerRoe2D(nx, ny, QM, QP, gamma)
% Purpose: compute surface fluxes for Euler's equations using the approximate 
%          Riemann solver based on Roe averages

% Rotate "-" trace momentum to face normal-tangent coordinates
rhouM = QM(:,:,2); rhovM = QM(:,:,3);
QM(:,:,2) = nx.*rhouM + ny.*rhovM; QM(:,:,3) =-ny.*rhouM + nx.*rhovM;

% Rotate "+" trace momentum to face normal-tangent coordinates
rhouP = QP(:,:,2); rhovP = QP(:,:,3);
QP(:,:,2) = nx.*rhouP + ny.*rhovP; QP(:,:,3) =-ny.*rhouP + nx.*rhovP;
 
% Compute fluxes and primitive variables in rotated coordinates
[fxQM,fyQM,rhoM,uM,vM,pM] = EulerFluxes2D(QM, gamma);
[fxQP,fyQP,rhoP,uP,vP,pP] = EulerFluxes2D(QP, gamma);
EnerM = QM(:,:,4); EnerP = QP(:,:,4);

Nfields = 4;
HM   = (EnerM+pM)./rhoM; cM = sqrt(gamma*pM./rhoM);
HP   = (EnerP+pP)./rhoP; cP = sqrt(gamma*pP./rhoP);

% Compute Roe average variables
rhoMs = sqrt(rhoM); rhoPs = sqrt(rhoP);

rho = rhoMs.*rhoPs;
u   = (rhoMs.*uM + rhoPs.*uP)./(rhoMs + rhoPs);
v   = (rhoMs.*vM + rhoPs.*vP)./(rhoMs + rhoPs);
H   = (rhoMs.*HM + rhoPs.*HP)./(rhoMs + rhoPs);

c2  = (gamma-1)*(H - 0.5*(u.^2 + v.^2)); c = sqrt(c2);

% Compute estimate of waves speeds
SL = min(uM-cM, u-c); SR = max(uP+cP, u+c);

% Compute HLL flux
t1 = (min(SR,0)-min(0,SL))./(SR-SL);
t2 = 1-t1;
t3 = (SR.*abs(SL)-SL.*abs(SR))./(2*(SR-SL));

for n=1:4
  fx(:,:,n) = t1.*fxQP(:,:,n) + t2.*fxQM(:,:,n) - t3.*(QP(:,:,n)-QM(:,:,n));
end

% rotate flux back into Cartesian coordinates
flux(:,:,1) = fx(:,:,1);
flux(:,:,2) = nx.*fx(:,:,2) - ny.*fx(:,:,3);
flux(:,:,3) = ny.*fx(:,:,2) + nx.*fx(:,:,3);
flux(:,:,4) = fx(:,:,4);;
return;
