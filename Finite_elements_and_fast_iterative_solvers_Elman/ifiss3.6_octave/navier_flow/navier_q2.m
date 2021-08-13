function n = navier_q2(xy,mv,flowsol,tout)
%NAVIER_Q2 Q2 convection matrix 
%   N = navier_q2(xy,ev,flowsol,tout);
%   input
%          xy         Q2 nodal coordinate vector 
%          ev         element mapping matrix
%          flowsol    Q2-Q1 or Q2-P1 flow solution
%          tout       output on/off switch (optional) 
%   output
%          N          Q2 scalar convection matrix
%
%   Natural boundary conditions apply.
%   IFISS function: DJS; 24 November 2009.
% Copyright (c) 2009 D.J. Silvester, H.C. Elman, A. Ramage 
if nargin < 4, tout = 1; end
nngpt=9; 
x=xy(:,1); y=xy(:,2);
nvtx=length(x);   nel=length(mv(:,1));
usol=flowsol(1:nvtx); vsol=flowsol(nvtx+1:2*nvtx); 
if tout==1,
fprintf('setting up Q2 convection matrix...  ')
end
%
% initialise global matrices
      n = sparse(nvtx,nvtx);
%
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
        for idx = 1:9		
		xsl(:,idx) = usol(mv(:,idx));
		ysl(:,idx) = vsol(mv(:,idx));
		end
      ne = zeros(nel,9,9);
% 
% loop over Gauss points
         for igpt = 1:nngpt
         sigpt=s(igpt);
         tigpt=t(igpt);
         wght=wt(igpt);
%  evaluate derivatives etc
         [jac,invjac,phi,dphidx,dphidy] = deriv(sigpt,tigpt,xl_v,yl_v);
         [psi,dpsidx,dpsidy] = qderiv(sigpt,tigpt,xl_v,yl_v); 
         u_x = zeros(nel,1); u_y=zeros(nel,1);
            for k=1:9
		    u_x(:) = u_x(:) + xsl(:,k) .* psi(:,k);
		    u_y(:) = u_y(:) + ysl(:,k) .* psi(:,k);	 
		    end
		for j = 1:9
            for i = 1:9               
				ne(:,i,j)  = ne(:,i,j)  + wght*u_x(:).*psi(:,i).*dpsidx(:,j);
                ne(:,i,j)  = ne(:,i,j)  + wght*u_y(:).*psi(:,i).*dpsidy(:,j);
             end
	    end
%
% end of Gauss point loop
         end  
%
%%  element assembly into global matrix
      for krow=1:9
	  nrow=mv(:,krow);	 
          for kcol=1:9
		  ncol=mv(:,kcol);	  
          n = n + sparse(nrow,ncol,ne(:,krow,kcol),nvtx,nvtx);
	      end
       end
%
if tout==1, fprintf('done.\n'), end
return
