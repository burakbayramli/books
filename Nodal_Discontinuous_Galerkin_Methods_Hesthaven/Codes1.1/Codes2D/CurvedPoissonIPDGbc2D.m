function [BOP] = CurvedPoisson2DIPDGbc2D()

% function [BOP] = CurvedPoissonIPDGbc2D()
% Purpose: Set up the discrete Poisson matrix directly
%          using LDG. The operator is set up in the weak form
Globals2D;

NGauss = gauss.NGauss;
% build DG derivative matrices
BOP = zeros(K*Np*Np*(1+Nfaces), 3); 

% global node numbering
entries = (1:Np*NGauss)'; cols1 = ones(Np,1)*(1:NGauss);
for k1=1:K 
  if(~mod(k1,200)) k1, end;
  rows1 = ((k1-1)*Np+1:k1*Np)'*ones(1,NGauss); 

  % Build element-to-element parts of operator
  for f1=1:Nfaces
    if(BCType(k1,f1))
      idsM = (f1-1)*NGauss+1:f1*NGauss;
      
      VM = gauss.finterp(:,:,f1);
      [dVdxM, dVdyM] = PhysDmatrices2D(x(:,k1), y(:,k1),VM);
      gnx = spdiags(gauss.nx(idsM, k1), 0, NGauss, NGauss);
      gny = spdiags(gauss.ny(idsM, k1), 0, NGauss, NGauss);
      gw  = spdiags(gauss.W(idsM, k1),  0, NGauss, NGauss);
      
      DnM = gnx*dVdxM + gny*dVdyM;
      
      hinv = Fscale( 1 + (f1-1)*Nfp, k1);
      
      BOP11 = zeros(Np,Nfp);
      
      gtau = 20*(N+1)*(N+1)*hinv; % set penalty scaling
      switch(BCType(k1,f1))
	case {Dirichlet}
	  BOP11 = ( VM'*gw*gtau - DnM'*gw );
	case {Neuman}
	  BOP11 = (  VM'*gw );	 % recent sign change
      end 

      BOP(entries(:), :)   = [rows1(:), cols1(:), BOP11(:)]; entries = entries + Np*NGauss;
    end 
    cols1 = cols1 + NGauss;
  end  
end  
Nentries = max(max(entries))-Np*NGauss;
BOP = BOP(1:Nentries,:);
BOP = myspconvert(BOP, Np*K, NGauss*Nfaces*K, 1e-15);
return
