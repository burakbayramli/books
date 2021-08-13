function [xjmp,yjmp,els] = stressjmps_q1p0(viscosity,q1p0sol,xy,ev,ebound);
%STRESSJMPS_Q1P0 stress jumps for rectangular Q1-P0 grid 
%   [xjmp,yjmp,els] = stressjmps_q1p0(viscosity,q1p0sol,xy,ev,ebound)
%   input
%          viscosity    (not used for Stokes flow)
%          q1p0sol      vertex solution vector
%          xy           vertex coordinate vector  
%          ev           element mapping matrix
%          ebound       element edge boundary matrix 
%   output 
%          xjmp, yjmp   component elementwise edge flux jumps
%          els          elementwise edge lengths
%
%   IFISS function: DJS; 8 March 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
      x=xy(:,1); y=xy(:,2); nvtx=length(x);
      nel=length(ev(:,1)); 
	  usol=q1p0sol(1:nvtx); vsol=q1p0sol(nvtx+1:2*nvtx); 
	  psol=q1p0sol(2*nvtx+1:end);
      fprintf('computing Q1-P0 element stress flux jumps...  ')
%
% initialise global matrices
      xflux_adj = sparse(nvtx,nvtx);  yflux_adj = sparse(nvtx,nvtx);
	  he_adj = sparse(nvtx,nvtx); 
      xjmp = zeros(nel,4);  yjmp = zeros(nel,4);
      els = zeros(nel,4);
	  zero_v=zeros(nel,1); one_v=ones(nel,1);
%
% inner loop over elements    
        for ivtx = 1:4
        xl_v(:,ivtx) = x(ev(:,ivtx));
        yl_v(:,ivtx) = y(ev(:,ivtx)); 
        xsl_v(:,ivtx) = usol(ev(:,ivtx));
		ysl_v(:,ivtx) = vsol(ev(:,ivtx));
		end
% evaluate derivatives and normal flux on each edge in turn
% and assemble into flux and edge length adjacency matrices 
%% nx= 0, ny=-1    
    [jac,invjac,phi,dphidx,dphidy] = deriv(0,-1,xl_v,yl_v);
    xfx_v=zero_v;  yfx_v=zero_v;
    for  ivtx=1:4 		 
	xfx_v = xfx_v +(-dphidy(:,ivtx)).*invjac(:).*xsl_v(:,ivtx);
    yfx_v = yfx_v +(-dphidy(:,ivtx)).*invjac(:).*ysl_v(:,ivtx);
    end 
	     xflux_adj=xflux_adj + sparse(ev(:,1),ev(:,2),viscosity*xfx_v,nvtx,nvtx);
		 yflux_adj=yflux_adj + sparse(ev(:,1),ev(:,2),viscosity*yfx_v+psol,nvtx,nvtx); 
         hx_v=xl_v(:,2)-xl_v(:,1); hy_v=yl_v(:,2)-yl_v(:,1); 
		 he_v=sqrt(hx_v.*hx_v+hy_v.*hy_v);
		 he_adj=he_adj + sparse(ev(:,1),ev(:,2),he_v,nvtx,nvtx);  
%        
%% nx= 1, ny= 0
    [jac,invjac,phi,dphidx,dphidy] = deriv(1,0,xl_v,yl_v);
    xfx_v=zero_v; yfx_v=zero_v;
    for  ivtx=1:4 		 
	xfx_v = xfx_v +( dphidx(:,ivtx)).*invjac(:).*xsl_v(:,ivtx);
	yfx_v = yfx_v +( dphidx(:,ivtx)).*invjac(:).*ysl_v(:,ivtx);
    end  
         xflux_adj=xflux_adj + sparse(ev(:,2),ev(:,3),viscosity*xfx_v-psol,nvtx,nvtx); 
		 yflux_adj=yflux_adj + sparse(ev(:,2),ev(:,3),viscosity*yfx_v,nvtx,nvtx); 
         hx_v=xl_v(:,3)-xl_v(:,2); hy_v=yl_v(:,3)-yl_v(:,2); 
		 he_v=sqrt(hx_v.*hx_v+hy_v.*hy_v);
		 he_adj=he_adj + sparse(ev(:,2),ev(:,3),he_v,nvtx,nvtx);  
%        
%% nx= 0, ny= 1
    [jac,invjac,phi,dphidx,dphidy] = deriv(0,1,xl_v,yl_v);
    xfx_v=zero_v; yfx_v=zero_v;
    for  ivtx=1:4 		 
	xfx_v = xfx_v +( dphidy(:,ivtx)).*invjac(:).*xsl_v(:,ivtx);
	yfx_v = yfx_v +( dphidy(:,ivtx)).*invjac(:).*ysl_v(:,ivtx);
    end  
         xflux_adj=xflux_adj + sparse(ev(:,3),ev(:,4),viscosity*xfx_v,nvtx,nvtx);  
         yflux_adj=yflux_adj + sparse(ev(:,3),ev(:,4),viscosity*yfx_v-psol,nvtx,nvtx);  
         hx_v=xl_v(:,4)-xl_v(:,3); hy_v=yl_v(:,4)-yl_v(:,3); 
		 he_v=sqrt(hx_v.*hx_v+hy_v.*hy_v);
		 he_adj=he_adj + sparse(ev(:,3),ev(:,4),he_v,nvtx,nvtx);  
% 
%% nx=-1, ny= 0
    [jac,invjac,phi,dphidx,dphidy] = deriv(-1,0,xl_v,yl_v);
    xfx_v=zero_v; yfx_v=zero_v;
    for  ivtx=1:4 		 
	xfx_v = xfx_v +(-dphidx(:,ivtx)).*invjac(:).*xsl_v(:,ivtx);
	yfx_v = yfx_v +(-dphidx(:,ivtx)).*invjac(:).*ysl_v(:,ivtx);
    end  
         xflux_adj=xflux_adj + sparse(ev(:,4),ev(:,1),viscosity*xfx_v+psol,nvtx,nvtx); 
		 yflux_adj=yflux_adj + sparse(ev(:,4),ev(:,1),viscosity*yfx_v,nvtx,nvtx);  
         hx_v=xl_v(:,1)-xl_v(:,4); hy_v=yl_v(:,1)-yl_v(:,4); 
		 he_v=sqrt(hx_v.*hx_v+hy_v.*hy_v);
		 he_adj=he_adj + sparse(ev(:,4),ev(:,1),he_v,nvtx,nvtx);  
%

% compute flux jumps and equidistribute      
      xflux_jmp =xflux_adj+xflux_adj';
	  yflux_jmp =yflux_adj+yflux_adj';
%
% loop over elements    
      for ielem = 1:nel
% assemble jumps and edge lengths into element matrices
         xjmp(ielem,1)=(xflux_jmp(ev(ielem,1),ev(ielem,2)));
         yjmp(ielem,1)=(yflux_jmp(ev(ielem,1),ev(ielem,2)));
         els(ielem,1)=(he_adj(ev(ielem,1),ev(ielem,2)));
         xjmp(ielem,2)=(xflux_jmp(ev(ielem,2),ev(ielem,3)));
		 yjmp(ielem,2)=(yflux_jmp(ev(ielem,2),ev(ielem,3)));
         els(ielem,2)=(he_adj(ev(ielem,2),ev(ielem,3)));
         xjmp(ielem,3)=(xflux_jmp(ev(ielem,3),ev(ielem,4)));
		 yjmp(ielem,3)=(yflux_jmp(ev(ielem,3),ev(ielem,4)));
         els(ielem,3)=(he_adj(ev(ielem,3),ev(ielem,4)));
         xjmp(ielem,4)=(xflux_jmp(ev(ielem,4),ev(ielem,1)));
		 yjmp(ielem,4)=(yflux_jmp(ev(ielem,4),ev(ielem,1)));
         els(ielem,4)=(he_adj(ev(ielem,4),ev(ielem,1)));
      end
% 
% remove Dirichlet boundary edge contributions
      nbde=length(ebound(:,1));
      for k=1:nbde
         xjmp(ebound(k,1),ebound(k,2))=0; 
		 yjmp(ebound(k,1),ebound(k,2))=0;  
      end
%
      fprintf('done\n')
      return
