function [bc] = PoissonIPDGbc3D(ubc)

% function [bc] = PoissonIPDGbc3D(ubc)
% Purpose: Set up the discrete Poisson matrix directly
%          using IP. The operator is set up in the weak form

Globals3D;

% face mass matrix 1
Fm = Fmask(:,1); faceR = r(Fm); faceS = s(Fm); 
V2D = Vandermonde2D(N, faceR, faceS);
massEdge(Fm,Fm,1) = inv(V2D*V2D');

% face mass matrix 2
Fm = Fmask(:,2); faceR = r(Fm); faceT = t(Fm);
V2D = Vandermonde2D(N, faceR, faceT);
massEdge(Fm,Fm,2) = inv(V2D*V2D');

% face mass matrix 3
Fm = Fmask(:,3); faceS = s(Fm); faceT = t(Fm);
V2D = Vandermonde2D(N, faceS, faceT); 
massEdge(Fm,Fm,3) = inv(V2D*V2D');

% face mass matrix 4
Fm = Fmask(:,4); faceS = s(Fm); faceT = t(Fm);
V2D = Vandermonde2D(N, faceS, faceT); 
massEdge(Fm,Fm,4) = inv(V2D*V2D');

% build DG right hand side
bc = zeros(Np, K);

for k1=1:K 
  if(~mod(k1,1000)) k1, end;

  for f1=1:Nfaces
    
    if(EToE(k1,f1)==k1)
      
      Fm1 = Fmask(:,f1); 
      fidM  = (k1-1)*Nfp*Nfaces + (f1-1)*Nfp + (1:Nfp)';

      id = 1+(f1-1)*Nfp + (k1-1)*Nfp*Nfaces;
      lnx = nx(id);  lny = ny(id);  lnz = nz(id); 
      lsJ = sJ(id); hinv = Fscale(id);
      
      Dx = rx(1,k1)*Dr + sx(1,k1)*Ds + tx(1,k1)*Dt;  
      Dy = ry(1,k1)*Dr + sy(1,k1)*Ds + ty(1,k1)*Dt;
      Dz = rz(1,k1)*Dr + sz(1,k1)*Ds + tz(1,k1)*Dt;
      Dn1 = lnx*Dx + lny*Dy +lnz*Dz;
      
      mmE = lsJ*massEdge(:,:,f1);

      gtau = 2*(N+1)*(N+1)*hinv; % set penalty scaling

      bc(:,k1) = bc(:,k1) + (gtau*mmE(:,Fm1) - Dn1'*mmE(:,Fm1))*ubc(fidM);
	  
    end   
  end  
end
return
