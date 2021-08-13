function [xjmp,yjmp] = stressjmps_q2q1(viscosity,q2q1sol,eex,...
                                                 xy,mv,mp,mbound,neumannb,s)
%STRESSJMPS_Q2Q1 vectorised stress jumps for Q2-Q1 grid 
%[xjmp,yjmp] = stressjmps_q2q1(viscosity,q2q1sol,eex,...
%                                               xy,mv,mp,mbound,neumannb,s)
%   input
%          viscosity    (set to be one for Stokes flow)
%          q2q1sol      vertex solution vector
%          eex          presorted element connectivity array 
%          xy           vertex coordinate vector  
%          mv           element mapping matrix
%          mp           Q1 element mapping matrix
%          mbound       element edge boundary matrix 
%          neumannb     Neumann boundary element/edge array
%          s            quadrature point coordinate in (-1,1) 
%   output 
%          xjmp, yjmp   component elementwise edge flux jumps
%
%   IFISS function: DJS; 4 January 2011.
% Copyright (c) 2010 D.J. Silvester, Qifeng Liao 
      x=xy(:,1); y=xy(:,2); nvtx=length(x);
      mel=length(mv(:,1)); nel=mel;
	  usol=q2q1sol(1:nvtx); vsol=q2q1sol(nvtx+1:2*nvtx); 
	  nump=q2q1sol(2*nvtx+1:end);
    
%
% initialise global matrices
      xjmp = zeros(mel,4);  yjmp = zeros(mel,4);
      xflux = zeros(mel,4); yflux = zeros(mel,4);
	  zero_v=zeros(mel,1); one_v=ones(mel,1);
%
% inner loop over elements    
        for ivtx = 1:4
        xl_v(:,ivtx) = x(mv(:,ivtx));
        yl_v(:,ivtx) = y(mv(:,ivtx)); 
        p(:,ivtx)=nump(mp(:,ivtx));
        end
        for ivtx=1:9
        xsl_v(:,ivtx) = usol(mv(:,ivtx));
		ysl_v(:,ivtx) = vsol(mv(:,ivtx));
        end
% evaluate derivatives and normal flux on each edge in turn
% and assemble into flux and edge length adjacency matrices 
% nx= 0, ny=-1    
    [jac,invjac,phi,dphidx,dphidy] = deriv(s,-1,xl_v,yl_v);
    [psi,dpsidx,dpsidy] = qderiv(s,-1,xl_v,yl_v); 
    xfx_v=zero_v;  yfx_v=zero_v;
    psol=zero_v; 
    for  ivtx=1:9 		 
	xfx_v = xfx_v +(-dpsidy(:,ivtx)).*invjac(:).*xsl_v(:,ivtx);
    yfx_v = yfx_v +(-dpsidy(:,ivtx)).*invjac(:).*ysl_v(:,ivtx);
    end
    for  ivtx=1:4
        psol=psol+p(:,ivtx).*phi(:,ivtx);
    end
    xflux(:,1)=viscosity*xfx_v;
    yflux(:,1)=viscosity*yfx_v+psol; 
	 
%        
% nx= 1, ny= 0
    [jac,invjac,phi,dphidx,dphidy] = deriv(1,s,xl_v,yl_v);
    [psi,dpsidx,dpsidy] = qderiv(1,s,xl_v,yl_v); 
    xfx_v=zero_v; yfx_v=zero_v;
    psol=zero_v;
    for  ivtx=1:9 		 
	xfx_v = xfx_v +( dpsidx(:,ivtx)).*invjac(:).*xsl_v(:,ivtx);
	yfx_v = yfx_v +( dpsidx(:,ivtx)).*invjac(:).*ysl_v(:,ivtx);
    end
    for  ivtx=1:4
        psol=psol+p(:,ivtx).*phi(:,ivtx);
    end
     xflux(:,2)=viscosity*xfx_v-psol; 
     yflux(:,2)=viscosity*yfx_v; 
%        
% nx= 0, ny= 1
    [jac,invjac,phi,dphidx,dphidy] = deriv(s,1,xl_v,yl_v);
    [psi,dpsidx,dpsidy] = qderiv(s,1,xl_v,yl_v);
    xfx_v=zero_v; yfx_v=zero_v;
    psol=zero_v;
    for  ivtx=1:9 		 
	xfx_v = xfx_v +( dpsidy(:,ivtx)).*invjac(:).*xsl_v(:,ivtx);
	yfx_v = yfx_v +( dpsidy(:,ivtx)).*invjac(:).*ysl_v(:,ivtx);
    end  
    for  ivtx=1:4
        psol=psol+p(:,ivtx).*phi(:,ivtx);
    end
     xflux(:,3)=viscosity*xfx_v;  
     yflux(:,3)=viscosity*yfx_v-psol;     
% 
% nx=-1, ny= 0
    [jac,invjac,phi,dphidx,dphidy] = deriv(-1,s,xl_v,yl_v);
    [psi,dpsidx,dpsidy] = qderiv(-1,s,xl_v,yl_v); 
    xfx_v=zero_v; yfx_v=zero_v;
    psol=zero_v;
    for  ivtx=1:9 		 
	xfx_v = xfx_v +(-dpsidx(:,ivtx)).*invjac(:).*xsl_v(:,ivtx);
	yfx_v = yfx_v +(-dpsidx(:,ivtx)).*invjac(:).*ysl_v(:,ivtx);
    end  
    for  ivtx=1:4
        psol=psol+p(:,ivtx).*phi(:,ivtx);
    end
    xflux(:,4)=viscosity*xfx_v+psol; 
	yflux(:,4)=viscosity*yfx_v;  

%tic       
% evaluate flux jump on each edge in turn 
% nx= 0, ny=-1  
    xjmp(1:nel,1) = xflux(1:nel,1)+xflux(eex(1:nel,1),3);
    yjmp(1:nel,1) = yflux(1:nel,1)+yflux(eex(1:nel,1),3);
 
% nx= 1, ny=0  
    xjmp(1:nel,2) = xflux(1:nel,2)+xflux(eex(1:nel,2),4);
    yjmp(1:nel,2) = yflux(1:nel,2)+yflux(eex(1:nel,2),4);
%
% nx= 0, ny=1  
    xjmp(1:nel,3) = xflux(1:nel,3)+xflux(eex(1:nel,3),1);
    yjmp(1:nel,3) = yflux(1:nel,3)+yflux(eex(1:nel,3),1);
%
%
% nx= -1, ny=0  
    xjmp(1:nel,4) = xflux(1:nel,4)+xflux(eex(1:nel,4),2);
    yjmp(1:nel,4) = yflux(1:nel,4)+yflux(eex(1:nel,4),2);
%
% etime=toc;
%  fprintf('\nflux jump assembly took %9.4e seconds',etime) 
% 
% remove Dirichlet boundary edge contributions
      nbde=length(mbound(:,1));
      for k=1:nbde
         xjmp(mbound(k,1),mbound(k,2))=0; 
		 yjmp(mbound(k,1),mbound(k,2))=0;  
      end
%
% Neumann natural outflow boundary condition correction ..
% This is often neglected, because it is going to zero weakly.
% neumannb=[] means that the Neumann edge is treated as an interior edge
if min(size(neumannb))>0    
   [jac,invjac,phi,dphidx,dphidy] = deriv(1,s,xl_v,yl_v);
   [psi,dpsidx,dpsidy] = qderiv(1,s,xl_v,yl_v); 
   xfx_v=zero_v; yfx_v=zero_v;
   psol=zero_v;
   for  ivtx=1:9 		 
	    xfx_v = xfx_v +( dpsidx(:,ivtx)).*invjac(:).*xsl_v(:,ivtx);
	    yfx_v = yfx_v +( dpsidx(:,ivtx)).*invjac(:).*ysl_v(:,ivtx);
   end
   for  ivtx=1:4
         psol=psol+p(:,ivtx).*phi(:,ivtx);
   end    
   xjmp(neumannb(:,1),2)=2*(viscosity*xfx_v(neumannb(:,1))-psol(neumannb(:,1)));
   yjmp(neumannb(:,1),2)=2*(viscosity*yfx_v(neumannb(:,1)));
end

return
