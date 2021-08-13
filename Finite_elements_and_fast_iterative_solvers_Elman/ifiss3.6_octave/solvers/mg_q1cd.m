function [a,n,r,epe,eph,epw] = mg_q1cd(xy,ev)
%MG_Q1CD convection-diffusion matrix generator for GMG
%   [a,n,r,epe,eph,epw] = mg_q1cd(xy,ev)  
%   input
%           xy         vertex coordinate vector  
%           ev         element mapping matrix
%   output 
%           a          discrete diffusion operator
%           n          discrete convection operator
%           r          dummy variable
%           epe        viscosity normalised element peclet numbers 
%           eph        flow specific element lengths 
%           epw        centroid evaluated wind 
%
%   IFISS function: DJS; 5 January 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage

%   Analogous to femq1_cd
%
x=xy(:,1); y=xy(:,2);
nvtx=length(x);
nel=length(ev(:,1));
lx=max(x)-min(x); ly=max(y)-min(y);
hx=max(diff(x)); hy=max(diff(y));
%
% initialise global matrices
      a = sparse(nvtx,nvtx);
      n = sparse(nvtx,nvtx);
      r = sparse(nvtx,nvtx);
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
	    ne = zeros(nel,4,4);
% loop over 2x2 Gauss points
         for igpt = 1:4
         sigpt=s(igpt);
         tigpt=t(igpt);
%  evaluate derivatives etc
         [jac,invjac,phi,dphidx,dphidy] = deriv(sigpt,tigpt,xl_v,yl_v);
         [flowx,flowy] = gauss_transprt(sigpt,tigpt,xl_v,yl_v);
		 for j = 1:4
               for i = 1:4
               ae(:,i,j) = ae(:,i,j)  + dphidx(:,i).*dphidx(:,j) .* invjac(:);
               ae(:,i,j) = ae(:,i,j)  + dphidy(:,i).*dphidy(:,j) .* invjac(:);
               ne(:,i,j) = ne(:,i,j) + flowx(:) .* phi(:,i) .* dphidx(:,j);
               ne(:,i,j) = ne(:,i,j) + flowy(:) .* phi(:,i) .* dphidy(:,j);
               end
	    end
% end of Gauss point loop
         end
%
% perform assembly of global matrix  and source vector 
      for krow=1:4
	  nrow=ev(:,krow);	 
          for kcol=1:4
		  ncol=ev(:,kcol);	  
          a = a + sparse(nrow,ncol,ae(:,krow,kcol),nvtx,nvtx);
          n = n + sparse(nrow,ncol,ne(:,krow,kcol),nvtx,nvtx);
          end
      end
%
%
% computation of element Peclet number (at the centroid)         
% rectangle specific calculation here
      hx=abs(xl_v(:,2)-xl_v(:,1)); hy=abs(yl_v(:,3)-yl_v(:,2));
      [flowx,flowy] = gauss_transprt(0,0,xl_v,yl_v);
      flow_l2 = sqrt(flowx(:) .* flowx(:) + flowy(:) .* flowy(:));
        if all(flowx==0), flow_h=hy;
	elseif all(flowy==0), flow_h=hx;
		else
          angle = atan(abs(flowy./flowx));
          flow_h = min([hx./cos(angle),hy./sin(angle)],[],2);
        end
	  eph = flow_h;
      epe = flow_h.*flow_l2/2;
	  epw = flow_l2;
%
return
