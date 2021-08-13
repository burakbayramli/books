function [Nxx,Nxy,Nyx,Nyy] = newton_q2(xy,mv,flowsol)
%NEWTON_Q2 Q2 convection derivative matrices 
%   [Nxx,Nxy,Nyx,Nyy] = newton_q2(xy,ev,flowsol);;
%   input
%          xy           Q2 nodal coordinate vector 
%          ev           element mapping matrix
%          flowsol      Q2-Q1 or Q2-P1 flow solution
%   output
%   Nxx, Nxy, Nyx, Nyy  Q2 scalar convection derivative matrices
%
%   Natural boundary conditions apply. 
%   IFISS function: DJS; 11 March 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
nngpt=9; 
x=xy(:,1); y=xy(:,2);
nvtx=length(x);   nel=length(mv(:,1));
usol=flowsol(1:nvtx); vsol=flowsol(nvtx+1:2*nvtx); 
%
fprintf('setting up Q2 Newton Jacobian matrices...  ')
%
% initialise global matrices
      Nxx = sparse(nvtx,nvtx);
      Nxy = sparse(nvtx,nvtx);
	  Nyx = sparse(nvtx,nvtx);
      Nyy = sparse(nvtx,nvtx);

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
      nxxe = zeros(nel,9,9); nxye=zeros(nel,9,9);
	  nyxe = zeros(nel,9,9); nyye=zeros(nel,9,9);
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
         ux_x = zeros(nel,1); ux_y=zeros(nel,1);
		 uy_x = zeros(nel,1); uy_y=zeros(nel,1);
            for k=1:9
		    ux_x(:) = ux_x(:) + xsl(:,k) .* dpsidx(:,k);
            ux_y(:) = ux_y(:) + xsl(:,k) .* dpsidy(:,k);
		    uy_x(:) = uy_x(:) + ysl(:,k) .* dpsidx(:,k);
            uy_y(:) = uy_y(:) + ysl(:,k) .* dpsidy(:,k);
		    end
% compute element mass matrices
			for j = 1:9
            for i = 1:9   
			ne(:,i,j)  =  wght*psi(:,i).*psi(:,j);
			nxxe(:,i,j)  = nxxe(:,i,j)  + ne(:,i,j).*ux_x(:);
			nxye(:,i,j)  = nxye(:,i,j)  + ne(:,i,j).*ux_y(:);
			nyxe(:,i,j)  = nyxe(:,i,j)  + ne(:,i,j).*uy_x(:);
			nyye(:,i,j)  = nyye(:,i,j)  + ne(:,i,j).*uy_y(:);
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
          Nxx = Nxx + sparse(nrow,ncol,nxxe(:,krow,kcol),nvtx,nvtx);
		  Nxy = Nxy + sparse(nrow,ncol,nxye(:,krow,kcol),nvtx,nvtx);
          Nyx = Nyx + sparse(nrow,ncol,nyxe(:,krow,kcol),nvtx,nvtx);
		  Nyy = Nyy + sparse(nrow,ncol,nyye(:,krow,kcol),nvtx,nvtx);
	      end
       end
%
fprintf('done.\n')
return
