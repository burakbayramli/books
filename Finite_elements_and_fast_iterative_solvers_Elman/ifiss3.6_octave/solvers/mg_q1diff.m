function [a,r,f] = mg_q1diff(xy,ev)
%MG_Q1DIFF bilinear diffusion matrix generator for GMG 
%   [a,r,f] = mg_q1diff(xy,ev);
%   input
%          xy         vertex coordinate vector  
%          ev         element mapping matrix
%   output
%          a          stiffness matrix
%          r          dummy variable 
%          f          rhs vector
%
%   IFISS function: DJS; 5 January 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage

x=xy(:,1); y=xy(:,2);
nvtx=length(x);
nel=length(ev(:,1));
lx=max(x)-min(x); ly=max(y)-min(y);
hx=max(diff(x)); hy=max(diff(y));
%
% initialise global matrices
      a = sparse(nvtx,nvtx);
      r = sparse(nvtx,nvtx);
      f = zeros(nvtx,1);
%
% set up 2x2 Gauss points
      gpt=1.0e0/sqrt(3.0e0);
      s(1) = -gpt;  t(1) = -gpt;
      s(2) =  gpt;  t(2) = -gpt;
      s(3) =  gpt;  t(3) =  gpt;
      s(4) = -gpt;  t(4) =  gpt;
%
% inner loop over elements    
        for ivtx = 1:4
        xl_v(:,ivtx) = x(ev(:,ivtx));
        yl_v(:,ivtx) = y(ev(:,ivtx)); 
		end
        ae = zeros(nel,4,4);
%  loop over 2x2 Gauss points
         for igpt = 1:4
         sigpt=s(igpt);
         tigpt=t(igpt);
%  evaluate derivatives etc
         [jac,invjac,phi,dphidx,dphidy] = deriv(sigpt,tigpt,xl_v,yl_v);
            for j = 1:4
               for i = 1:4
               ae(:,i,j) = ae(:,i,j)  + dphidx(:,i).*dphidx(:,j) .* invjac(:);
               ae(:,i,j) = ae(:,i,j)  + dphidy(:,i).*dphidy(:,j) .* invjac(:);
               end
 	        end
% end of Gauss point loop
         end
% perform assembly of global matrix  and source vector 
      for krow=1:4
	  nrow=ev(:,krow);	 
          for kcol=1:4
		  ncol=ev(:,kcol);	  
          a = a + sparse(nrow,ncol,ae(:,krow,kcol),nvtx,nvtx);
          end
      end
%
