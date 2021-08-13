function n = navier_q1(xy,ev,flowsol,tout)
%NAVIER_Q1 Q1 convection matrix 
%   N = navier_q1(xy,ev,flowsol,tout);
%   input
%          xy         Q2 nodal coordinate vector 
%          ev         element mapping matrix
%          flowsol    Q1-Q1 or Q1_P0 flow solution
%          tout       output on/off switch (optional)   
%   output
%          N          Q1 scalar convection matrix
%
%   Natural boundary conditions apply. 
%   IFISS function: DJS;  24 November 2009.
% Copyright (c) 2009 D.J. Silvester, H.C. Elman, A. Ramage 
if nargin < 4, tout = 1; end
nngpt=4; 
x=xy(:,1); y=xy(:,2);
nvtx=length(x);   nel=length(ev(:,1));
usol=flowsol(1:nvtx); vsol=flowsol(nvtx+1:2*nvtx); 
if tout==1,
fprintf('setting up Q1 convection matrix...  ')
end
%
% initialise global matrices
      n = sparse(nvtx,nvtx);
%
% Gauss point integration rules
      if (nngpt==4)        % 2x2 Gauss points
      gpt=1.0e0/sqrt(3.0e0);
      s(1) = -gpt; t(1) = -gpt; wt(1)=1;
      s(2) =  gpt; t(2) = -gpt; wt(2)=1;
      s(3) =  gpt; t(3) =  gpt; wt(3)=1; 
      s(4) = -gpt; t(4) =  gpt; wt(4)=1;
      elseif (nngpt==1)   % 1x1 Gauss point
      s(1) =    0; t(1) =    0; wt(1)=4;
      else
	  error('Check Gauss point integration specification')
      end
%
% inner loop over elements    
      for ivtx = 1:4
      xl_v(:,ivtx) = x(ev(:,ivtx));
      yl_v(:,ivtx) = y(ev(:,ivtx)); 
      xsl(:,ivtx) = usol(ev(:,ivtx));
	  ysl(:,ivtx) = vsol(ev(:,ivtx));
	  end
      ne = zeros(nel,4,4);
% 
% loop over Gauss points
         for igpt = 1:nngpt
         sigpt=s(igpt);
         tigpt=t(igpt);
         wght=wt(igpt);
%  evaluate derivatives etc
         [jac,invjac,phi,dphidx,dphidy] = deriv(sigpt,tigpt,xl_v,yl_v);
         u_x = zeros(nel,1); u_y=zeros(nel,1);
            for k=1:4
		    u_x(:) = u_x(:) + xsl(:,k) .* phi(:,k);
		    u_y(:) = u_y(:) + ysl(:,k) .* phi(:,k);	 
		    end
		 for j = 1:4
            for i = 1:4               
				ne(:,i,j)  = ne(:,i,j)  + wght*u_x(:).*phi(:,i).*dphidx(:,j);
                ne(:,i,j)  = ne(:,i,j)  + wght*u_y(:).*phi(:,i).*dphidy(:,j);
             end
	    end
%
% end of Gauss point loop
         end  
%
%%  element assembly into global matrix
      for krow=1:4
	  nrow=ev(:,krow);	 
          for kcol=1:4
		  ncol=ev(:,kcol);	  
          n = n + sparse(nrow,ncol,ne(:,krow,kcol),nvtx,nvtx);
	      end
       end
%
if tout==1, fprintf('done.\n'), end
return
