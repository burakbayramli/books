function d = femq1_cd_supg(xy,ev,expe,eph,epw)
%FEMQ1_CD_SUPG vectorized Q1 streamline diffusion matrix generator
%    S = femq1_cd_supg(xy,ev,expe,eph,epw);
%   input
%          xy       vertex coordinate vector  
%          ev       element mapping matrix
%          expe     element peclet numbers 
%          eph      flow specific element lengths 
%          epw      centroid evaluated wind 
%   output
%          S        streamline diffusion matrix
%   Natural boundary conditions apply. Dirichlet conditions
%   must be explicitly enforced by calling function nonzerobc.
%   IFISS function: DJS; 3 April 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
x=xy(:,1); y=xy(:,2);
nvtx=length(x);
nel=length(ev(:,1));
fprintf('setting up Q1 SUPG stabilisation matrix...  ')
% initialise global matrices
      d = sparse(nvtx,nvtx);
%
% find the elements where streamline diffusion is active
acte = find(isfinite(expe)); nacte=length(acte);
if length(acte)==0, 
fprintf('not needed!\n'), return, end
%
% initialise global matrices
      d = sparse(nvtx,nvtx);
%
% set up 2x2 Gauss points
      gpt=1.0e0/sqrt(3.0e0);
      s(1) = -gpt;  t(1) = -gpt;
      s(2) =  gpt;  t(2) = -gpt;
      s(3) =  gpt;  t(3) =  gpt;
      s(4) = -gpt;  t(4) =  gpt;
%
% loop over active and inactive elements    
%      for iact = 1:nacte
% ielem = acte(iact);
% inner loop over elements    
        for ivtx = 1:4
        xl_v(:,ivtx) = x(ev(:,ivtx));
        yl_v(:,ivtx) = y(ev(:,ivtx)); 
		end
        de = zeros(nel,4,4);
% loop over 2x2 Gauss points
         for igpt = 1:4
         sigpt=s(igpt);
         tigpt=t(igpt);
%  evaluate derivatives etc
         [jac,invjac,phi,dphidx,dphidy] = deriv(sigpt,tigpt,xl_v,yl_v);
         [flowx,flowy] = gauss_transprt(sigpt,tigpt,xl_v,yl_v);
		 for j = 1:4
               for i = 1:4
    de(:,i,j) = de(:,i,j) + flowx(:).*dphidx(:,i).*flowx(:).*dphidx(:,j).*invjac(:);
    de(:,i,j) = de(:,i,j) + flowy(:).*dphidy(:,i).*flowx(:).*dphidx(:,j).*invjac(:);
    de(:,i,j) = de(:,i,j) + flowx(:).*dphidx(:,i).*flowy(:).*dphidy(:,j).*invjac(:);
    de(:,i,j) = de(:,i,j) + flowy(:).*dphidy(:,i).*flowy(:).*dphidy(:,j).*invjac(:);
               end
	    end
% end of Gauss point loop
         end
%
% scale with the appropriate parameter
      acte = find(isfinite(expe));
      factor = expe(acte); flow_h=eph(acte); flow_l2=epw(acte);
	  lpe =zeros(nel,1); lpe(acte)= factor.*(flow_h./flow_l2);
		 for j = 1:4
               for i = 1:4
               de(:,i,j) = lpe(:) .* de(:,i,j);
		   end
	   end
%   
% perform assembly of global matrix  and source vector 
      for krow=1:4
	  nrow=ev(:,krow);	 
          for kcol=1:4
		  ncol=ev(:,kcol);	  
          d = d + sparse(nrow,ncol,de(:,krow,kcol),nvtx,nvtx);
          end
      end
%
fprintf('done\n')
return
