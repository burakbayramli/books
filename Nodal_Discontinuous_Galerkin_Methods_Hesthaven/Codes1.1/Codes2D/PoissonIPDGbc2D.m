function [bc] = PoissonIPDGbc2D(ubc,qbc)

% Purpose: Set up the discrete Poisson matrix directly
%          using LDG. The operator is set up in the weak form
Globals2D;

% build local face matrices
massEdge = zeros(Np,Np,Nfaces);
Fm = Fmask(:,1); faceR = r(Fm); V1D = Vandermonde1D(N, faceR);  massEdge(Fm,Fm,1) = inv(V1D*V1D');
Fm = Fmask(:,2); faceR = r(Fm); V1D = Vandermonde1D(N, faceR);  massEdge(Fm,Fm,2) = inv(V1D*V1D');
Fm = Fmask(:,3); faceS = s(Fm); V1D = Vandermonde1D(N, faceS);  massEdge(Fm,Fm,3) = inv(V1D*V1D');

% build DG right hand side
bc = zeros(Np, K);

for k1=1:K 
  if(~mod(k1,1000)) k1, end;

  for f1=1:Nfaces
    
    if(BCType(k1,f1))
      
      Fm1 = Fmask(:,f1); 
      fidM  = (k1-1)*Nfp*Nfaces + (f1-1)*Nfp + (1:Nfp)';

      id = 1+(f1-1)*Nfp + (k1-1)*Nfp*Nfaces;
      lnx = nx(id);  lny = ny(id); lsJ = sJ(id); hinv = Fscale(id);
      
      Dx = rx(1,k1)*Dr + sx(1,k1)*Ds;  
      Dy = ry(1,k1)*Dr + sy(1,k1)*Ds;
      Dn1 = lnx*Dx  + lny*Dy ;
      
      mmE = lsJ*massEdge(:,:,f1);

      gtau = 100*2*(N+1)*(N+1)*hinv; % set penalty scaling
      switch(BCType(k1,f1))
	    case {Dirichlet}
	      bc(:,k1) = bc(:,k1) + (gtau*mmE(:,Fm1) - Dn1'*mmE(:,Fm1))*ubc(fidM);
		case {Neuman}
	      bc(:,k1) = bc(:,k1) + mmE(:,Fm1)*qbc(fidM);
		otherwise
	  end 
    end   
  end  
end
return
