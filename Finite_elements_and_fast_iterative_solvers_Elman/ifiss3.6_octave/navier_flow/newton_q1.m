function [Nxx,Nxy,Nyx,Nyy] = newton_q1(xy,ev,flowsol)
%NEWTON_Q1 Q1 convection derivative matrices 
%   [Nxx,Nxy,Nyx,Nyy] = newton_q1(xy,ev,flowsol);;
%   input
%          xy           Q2 nodal coordinate vector 
%          ev           element mapping matrix
%          flowsol      Q1-Q1 or Q1-P0 flow solution
%   output
%   Nxx, Nxy, Nyx, Nyy  Q1 scalar convection derivative matrices
%
%   Natural boundary conditions apply. 
%   IFISS function: DJS; 11 March 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
nngpt=4; 
x=xy(:,1); y=xy(:,2);
nvtx=length(x);   nel=length(ev(:,1));
usol=flowsol(1:nvtx); vsol=flowsol(nvtx+1:2*nvtx); 
fprintf('setting up Q1 Newton Jacobian matrices...  ')
%
% initialise global matrices
      Nxx = sparse(nvtx,nvtx);
      Nxy = sparse(nvtx,nvtx);
	  Nyx = sparse(nvtx,nvtx);
      Nyy = sparse(nvtx,nvtx);
%
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
      nxxe = zeros(nel,4,4); nxye=zeros(nel,4,4);
	  nyxe = zeros(nel,4,4); nyye=zeros(nel,4,4);
      ne = zeros(nel,4,4);
% 
% loop over Gauss points
         for igpt = 1:nngpt
         sigpt=s(igpt);
         tigpt=t(igpt);
         wght=wt(igpt);
%  evaluate derivatives etc
         [jac,invjac,phi,dphidx,dphidy] = deriv(sigpt,tigpt,xl_v,yl_v);
         ux_x = zeros(nel,1); ux_y=zeros(nel,1);
		 uy_x = zeros(nel,1); uy_y=zeros(nel,1);
            for k=1:4
		    ux_x(:) = ux_x(:) + xsl(:,k) .* dphidx(:,k);
            ux_y(:) = ux_y(:) + xsl(:,k) .* dphidy(:,k);
		    uy_x(:) = uy_x(:) + ysl(:,k) .* dphidx(:,k);
            uy_y(:) = uy_y(:) + ysl(:,k) .* dphidy(:,k);
		    end
% compute element mass matrices
			for j = 1:4
            for i = 1:4   
			ne(:,i,j)  =  wght*phi(:,i).*phi(:,j);
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
      for krow=1:4
	  nrow=ev(:,krow);	 
          for kcol=1:4
		  ncol=ev(:,kcol);	  
          Nxx = Nxx + sparse(nrow,ncol,nxxe(:,krow,kcol),nvtx,nvtx);
		  Nxy = Nxy + sparse(nrow,ncol,nxye(:,krow,kcol),nvtx,nvtx);
          Nyx = Nyx + sparse(nrow,ncol,nyxe(:,krow,kcol),nvtx,nvtx);
		  Nyy = Nyy + sparse(nrow,ncol,nyye(:,krow,kcol),nvtx,nvtx);
	      end
       end
%
fprintf('done.\n')
return
