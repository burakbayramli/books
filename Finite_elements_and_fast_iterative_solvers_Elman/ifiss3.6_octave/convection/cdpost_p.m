function [elerr_p,fe,ae] = cdpost_p(viscosity,uh,jmp,els,xy,ev,ebound)
%CDPOST_P computes local Poisson error estimator for Q1 solution 
%   [elerr_p,fe,ae] = cdpost_p(viscosity,uh,jmp,els,xy,ev,ebound);
%   input
%          viscosity   viscosity parameter
%          uh          nodal solution vector
%          jmp         elementwise edge flux jumps
%          els         elementwise edge lengths
%          xy          vertex coordinate vector  
%          ev          element mapping matrix
%          ebound      element edge boundary matrix 
%
%   output
%          elerr_p     elementwise error estimate
%          fe          elementwise rhs vectors
%          ae          elementwise Poisson problem matrices
%
%   IFISS function: DJS; 5 March 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
      fprintf('computing local error estimator...  ')
      x=xy(:,1); y=xy(:,2);
      nel=length(ev(:,1));
      elerr_p=zeros(nel,1);
%
% set up 3x3 Gauss points
      gpt=sqrt(0.6); 
      s(1) = -gpt; t(1) = -gpt; wt(1)=25/81;
      s(2) =  gpt; t(2) = -gpt; wt(2)=25/81;
      s(3) =  gpt; t(3) =  gpt; wt(3)=25/81; 
      s(4) = -gpt; t(4) =  gpt; wt(4)=25/81;
      s(5) =  0.0; t(5) = -gpt; wt(5)=40/81;
      s(6) =  gpt; t(6) =  0.0; wt(6)=40/81;
      s(7) =  0.0; t(7) =  gpt; wt(7)=40/81; 
      s(8) = -gpt; t(8) =  0.0; wt(8)=40/81;
      s(9) =  0.0; t(9) =  0.0; wt(9)=64/81;
%
% inner loop over elements    
        for ivtx = 1:4
        xl_v(:,ivtx) = x(ev(:,ivtx));
        yl_v(:,ivtx) = y(ev(:,ivtx)); 
        uh_v(:,ivtx) = uh(ev(:,ivtx)); 
		end
        ae = zeros(nel,5,5); elerr=zeros(5,nel);
        fe = zeros(nel,5); 
% loop over Gauss points
         for igpt = 1:9
         sigpt=s(igpt);
         tigpt=t(igpt);
         wght=wt(igpt);
         [jac_v,invjac_v,phi_v,dphidx_v,dphidy_v] = deriv(sigpt,tigpt,xl_v,yl_v);
         [flowx,flowy] = gauss_transprt(sigpt,tigpt,xl_v,yl_v);
         [psi_v,dpsidx_v,dpsidy_v] = qderiv(sigpt,tigpt,xl_v,yl_v);        
         uhe=zeros(nel,1);
 % compute convective derivative of the solution at the Gauss-point 
        for ivtx = 1:4
        uhe(:)=uhe(:)+(flowx(:).*dphidx_v(:,ivtx)+flowy(:).*dphidy_v(:,ivtx)).*uh_v(:,ivtx);
		end
            for j = 1:5
               for i = 1:5
               ae(:,i,j) = ae(:,i,j)+wght*dpsidx_v(:,i+4).*dpsidx_v(:,j+4).*invjac_v(:);
               ae(:,i,j) = ae(:,i,j)+wght*dpsidy_v(:,i+4).*dpsidy_v(:,j+4).*invjac_v(:);
               end
	        fe(:,j) = fe(:,j) - wght*uhe(:) .* psi_v(:,j+4); 
            end
% end of Gauss point loop
         end
%
% include edge jumps (evaluated at the midpoint)
         for ee = 1:4
         fe(:,ee) = fe(:,ee) - viscosity*jmp(:,ee) .* els(:,ee)*(1/3);
         end
%
% solve for local estimate (sequential code)
         for ielem = 1:nel
		 elerr(:,ielem) = viscosity*squeeze(ae(ielem,1:5,1:5))\(fe(ielem,1:5)'); 
         end
         for ivtx=1:5, 
	     elerr_p(:) = elerr_p(:) + fe(:,ivtx) .* elerr(ivtx,:)'/viscosity;
         end
      return

