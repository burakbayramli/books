function [jmp,els] = oldq1fluxjmps(q1sol,xy,ev,ebound);
%OLDQ1FLUXJMPS legacy code | replaced by q1fluxjmps 
%   [jmp,els] = oldq1fluxjmps(q1sol,xy,ev,ebound);
%   input
%          q1sol      vertex solution vector
%          xy         vertex coordinate vector  
%          ev         element mapping matrix
%          ebound     element edge boundary matrix 
%   output
%          jmp       elementwise edge flux jumps
%          els       elementwise edge lengths
%
%   IFISS function: DJS; 4 March 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
      x=xy(:,1); y=xy(:,2); nvtx=length(x);
      nel=length(ev(:,1)); 
      fprintf('computing Q1 element flux jumps...  ')
%
% initialise global matrices
      flux_adj = sparse(nvtx,nvtx);  
      he_adj = sparse(nvtx,nvtx); 
      jmp = zeros(nel,4);
      els = zeros(nel,4);
	  zero_v=zeros(nel,1); one_v=ones(nel,1);
%
% inner loop over elements    
        for ivtx = 1:4
        xl_v(:,ivtx) = x(ev(:,ivtx));
        yl_v(:,ivtx) = y(ev(:,ivtx)); 
        sl_v(:,ivtx) = q1sol(ev(:,ivtx)); 
		end
% evaluate derivatives and normal flux on each edge in turn
% and assemble into flux and edge length adjacency matrices 
%% nx= 0, ny=-1    
    [jac,invjac,phi,dphidx,dphidy] = deriv(0,-1,xl_v,yl_v);
    fx_v=zero_v;
    for  ivtx=1:4 		 
	fx_v = fx_v +(-dphidy(:,ivtx)).*invjac(:).*sl_v(:,ivtx);
    end  
         flux_adj=flux_adj + sparse(ev(:,1),ev(:,2),fx_v,nvtx,nvtx);  
         hx_v=xl_v(:,2)-xl_v(:,1); hy_v=yl_v(:,2)-yl_v(:,1); 
		 he_v=sqrt(hx_v.*hx_v+hy_v.*hy_v);
		 he_adj=he_adj + sparse(ev(:,1),ev(:,2),he_v,nvtx,nvtx);  
%        
%% nx= 1, ny= 0
    [jac,invjac,phi,dphidx,dphidy] = deriv(1,0,xl_v,yl_v);
    fx_v=zero_v;
    for  ivtx=1:4 		 
	fx_v = fx_v +( dphidx(:,ivtx)).*invjac(:).*sl_v(:,ivtx);
    end  
         flux_adj=flux_adj + sparse(ev(:,2),ev(:,3),fx_v,nvtx,nvtx);  
         hx_v=xl_v(:,3)-xl_v(:,2); hy_v=yl_v(:,3)-yl_v(:,2); 
		 he_v=sqrt(hx_v.*hx_v+hy_v.*hy_v);
		 he_adj=he_adj + sparse(ev(:,2),ev(:,3),he_v,nvtx,nvtx);  
%        
%% nx= 0, ny= 1
    [jac,invjac,phi,dphidx,dphidy] = deriv(0,1,xl_v,yl_v);
    fx_v=zero_v;
    for  ivtx=1:4 		 
	fx_v = fx_v +( dphidy(:,ivtx)).*invjac(:).*sl_v(:,ivtx);
    end  
         flux_adj=flux_adj + sparse(ev(:,3),ev(:,4),fx_v,nvtx,nvtx);  
         hx_v=xl_v(:,4)-xl_v(:,3); hy_v=yl_v(:,4)-yl_v(:,3); 
		 he_v=sqrt(hx_v.*hx_v+hy_v.*hy_v);
		 he_adj=he_adj + sparse(ev(:,3),ev(:,4),he_v,nvtx,nvtx);  
% 
%% nx=-1, ny= 0
    [jac,invjac,phi,dphidx,dphidy] = deriv(-1,0,xl_v,yl_v);
    fx_v=zero_v;
    for  ivtx=1:4 		 
	fx_v = fx_v +(-dphidx(:,ivtx)).*invjac(:).*sl_v(:,ivtx);
    end  
         flux_adj=flux_adj + sparse(ev(:,4),ev(:,1),fx_v,nvtx,nvtx);  
         hx_v=xl_v(:,1)-xl_v(:,4); hy_v=yl_v(:,1)-yl_v(:,4); 
		 he_v=sqrt(hx_v.*hx_v+hy_v.*hy_v);
		 he_adj=he_adj + sparse(ev(:,4),ev(:,1),he_v,nvtx,nvtx);  
%

% compute flux jumps and equidistribute      
      flux_jmp =flux_adj+flux_adj';
%
% loop over elements    
      for ielem = 1:nel
% assemble jumps and edge lengths into element matrices
         jmp(ielem,1)=(flux_jmp(ev(ielem,1),ev(ielem,2)));
         els(ielem,1)=(he_adj(ev(ielem,1),ev(ielem,2)));
         jmp(ielem,2)=(flux_jmp(ev(ielem,2),ev(ielem,3)));
         els(ielem,2)=(he_adj(ev(ielem,2),ev(ielem,3)));
         jmp(ielem,3)=(flux_jmp(ev(ielem,3),ev(ielem,4)));
         els(ielem,3)=(he_adj(ev(ielem,3),ev(ielem,4)));
         jmp(ielem,4)=(flux_jmp(ev(ielem,4),ev(ielem,1)));
         els(ielem,4)=(he_adj(ev(ielem,4),ev(ielem,1)));
      end
% 
% remove Dirichlet boundary edge contributions
      nbde=length(ebound(:,1));
      for k=1:nbde
         jmp(ebound(k,1),ebound(k,2))=0;  
      end
%
      fprintf('done\n')
      return
