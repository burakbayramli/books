function [a,r,f] = femq2_diff(xy,mv)
%FEMQ2_DIFF vectorized biquadratic coefficient matrix generator
%   [A,Q,f] = femq2_diff(xy,mv);
%   input
%          xy         nodal coordinate vector  
%          mv         element mapping matrix
%   output
%          A          stiffness matrix
%          Q          mass matrix 
%          f          rhs vector
%
%   Natural boundary conditions apply. Dirichlet conditions
%   must be explicitly enforced by calling function nonzerobc.
%   IFISS function: DJS; 4 March 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
x=xy(:,1); y=xy(:,2);
nvtx=length(x);
nel=length(mv(:,1));
lx=max(x)-min(x); ly=max(y)-min(y);
hx=max(diff(x)); hy=max(diff(y));
fprintf('setting up Q2 diffusion matrices...  ')
%
% initialise global matrices
      a = sparse(nvtx,nvtx);
      r = sparse(nvtx,nvtx);
      f = zeros(nvtx,1);
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
        xl_v(:,ivtx) = x(mv(:,ivtx));
        yl_v(:,ivtx) = y(mv(:,ivtx)); 
		end
        ae = zeros(nel,9,9);
        re = zeros(nel,9,9);
        fe = zeros(nel,9);
% loop over 3x3 Gauss points
        for igpt = 1:9
        sigpt=s(igpt);
        tigpt=t(igpt);
        wght=wt(igpt);
%  evaluate derivatives etc
         [jac,invjac,phi,dphidx,dphidy] = deriv(sigpt,tigpt,xl_v,yl_v);
         rhs = gauss_source(sigpt,tigpt,xl_v,yl_v);
         [psi,dpsidx,dpsidy] = qderiv(sigpt,tigpt,xl_v,yl_v);        
            for j = 1:9
               for i = 1:9
               ae(:,i,j)  = ae(:,i,j)  + wght*dpsidx(:,i).*dpsidx(:,j).*invjac(:);
               ae(:,i,j)  = ae(:,i,j)  + wght*dpsidy(:,i).*dpsidy(:,j).*invjac(:);
               re(:,i,j)  = re(:,i,j)  + wght*psi(:,i).*psi(:,j).*jac(:);
               end
	        fe(:,j) = fe(:,j)  +  wght* rhs(:) .* psi(:,j) .* jac(:); 
            end
% end of Gauss point loop
         end
%
% perform assembly of global matrix  and source vector 
      for krow=1:9
	  nrow=mv(:,krow);	 
          for kcol=1:9
		  ncol=mv(:,kcol);	  
          a = a + sparse(nrow,ncol,ae(:,krow,kcol),nvtx,nvtx);
          r = r + sparse(nrow,ncol,re(:,krow,kcol),nvtx,nvtx);
          end
      f(nrow,1) = f(nrow,1) + fe(:,krow);
      end
%
%
fprintf('done\n')
return

