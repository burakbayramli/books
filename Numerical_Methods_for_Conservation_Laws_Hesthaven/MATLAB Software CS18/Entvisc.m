function [nu] = Entvisc(x,entold,entnew,fpold,fpnew,D,iV,N,m,h,k,VtoE,c1,c2);
% function [nu]=Entvisc(x,entold,entnew,fpold,fpnew,D,iV,N,m,h,k,VtoE,c1,c2);
% Purpose: Compute nonlinear viscosity following entropy approach
nu = zeros(m+1,N); nuh = zeros(1,N); onev = ones(m+1,1);
ente = zeros(N+2,2); fpe = zeros(N+2,2);

% Compute cell wise residual by Crank-Nicholson approximation
Resi = (entnew - entold)/k + (fpnew.*(D*entnew) + fpold.*(D*entold))/h;

% Compute interface jump
[ente] = extendDG(entnew(VtoE),'N',0,'N',0);
[fpe] = extendDG(fpnew(VtoE),'N',0,'N',0);
cL = (fpe(1:N,2)+fpe(2:N+1,1)).*(ente(1:N,2)-ente(2:N+1,1))/2;
cR = (fpe(2:N+1,2)+fpe(3:N+2,1)).*(ente(2:N+1,2)-ente(3:N+2,1))/2;
Ji = max(cL',cR')/h;

% Compute normalization
Eh = iV*entnew; NormE = max(abs(entnew - onev*Eh(1,:)));

% Define numerical viscosity
Di = max( max(abs(Resi)), abs(Ji)./NormE);
nuh = min(c1*h*max(abs(fpnew)), c2*h^2*Di);

% Compute continuous viscosity
nue = zeros(1,N+2); nue = [nuh(1) nuh nuh(N)];
maxL = max(nue(1:N),nue(2:N+1)); maxR = max(nue(2:N+1),nue(3:N+2));
nu = onev*maxL + (x-onev*x(1,:))/h.*(onev*(maxR-maxL));
return