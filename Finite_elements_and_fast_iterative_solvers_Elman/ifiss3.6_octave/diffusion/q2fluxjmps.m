function jmp = q2fluxjmps(q2sol,eex,xy,mv,mbound,neumannb,s)
%Q2FLUXJMPS vectorised flux jumps for rectangular Q2 grid 
%   jmp = q2fluxjmps(q2sol,xy,mv,mbound,neumannb,s);
%   input
%          q2sol        vertex solution vector
%          eex          presorted element connectivity array 
%          xy           vertex coordinate vector  
%          mv           element mapping matrix
%          mbound       element edge boundary matrix 
%          neumannb     Neumann boundary element/edge array
%          s            quadrature point coordinate in (-1,1) 
%   output 
%          jmp          component elementwise edge flux jumps
%
%   IFISS function: QL; 2 Nov 2010.
% Copyright (c) 2010 D.J. Silvester, Qifeng Liao 
      x=xy(:,1); y=xy(:,2); nvtx=length(x);
      mel=length(mv(:,1)); nel=mel;
      
% initialise global matrices
      jmp   = zeros(mel,4);
      flux  = zeros(mel,4);
	  zero_v=zeros(mel,1); one_v=ones(mel,1);
%
% inner loop over elements    
        for ivtx = 1:4
            xl_v(:,ivtx) = x(mv(:,ivtx));
            yl_v(:,ivtx) = y(mv(:,ivtx)); 
        end
        for ivtx=1:9
            sl_v(:,ivtx) = q2sol(mv(:,ivtx));
        end
% evaluate derivatives and normal flux on each edge in turn
% and assemble into flux and edge length adjacency matrices 
% nx= 0, ny=-1    
    [jac,invjac,phi,dphidx,dphidy] = deriv(s,-1,xl_v,yl_v);
    [psi,dpsidx,dpsidy] = qderiv(s,-1,xl_v,yl_v); 
    fx_v=zero_v;  
    for  ivtx=1:9 		 
	   fx_v = fx_v +(-dpsidy(:,ivtx)).*invjac(:).*sl_v(:,ivtx);
    end
    flux(:,1)=fx_v;
	 
%        
% nx= 1, ny= 0
    [jac,invjac,phi,dphidx,dphidy] = deriv(1,s,xl_v,yl_v);
    [psi,dpsidx,dpsidy] = qderiv(1,s,xl_v,yl_v); 
    fx_v=zero_v; 
    for  ivtx=1:9 		 
	   fx_v = fx_v +( dpsidx(:,ivtx)).*invjac(:).*sl_v(:,ivtx);
    end
    flux(:,2)=fx_v;
%        
% nx= 0, ny= 1
    [jac,invjac,phi,dphidx,dphidy] = deriv(s,1,xl_v,yl_v);
    [psi,dpsidx,dpsidy] = qderiv(s,1,xl_v,yl_v);
    fx_v=zero_v; 
    for  ivtx=1:9 		 
	   fx_v = fx_v +( dpsidy(:,ivtx)).*invjac(:).*sl_v(:,ivtx);
    end  
    flux(:,3)=fx_v;  
% 
% nx=-1, ny= 0
    [jac,invjac,phi,dphidx,dphidy] = deriv(-1,s,xl_v,yl_v);
    [psi,dpsidx,dpsidy] = qderiv(-1,s,xl_v,yl_v); 
    fx_v=zero_v; 
    for  ivtx=1:9 		 
	   fx_v = fx_v +(-dpsidx(:,ivtx)).*invjac(:).*sl_v(:,ivtx);
    end  
    flux(:,4)=fx_v; 

%tic       
% evaluate flux jump on each edge in turn 
% nx= 0, ny=-1  
    jmp(1:nel,1) = flux(1:nel,1)+flux(eex(1:nel,1),3); 
% nx= 1, ny=0  
    jmp(1:nel,2) = flux(1:nel,2)+flux(eex(1:nel,2),4);
%
% nx= 0, ny=1  
    jmp(1:nel,3) = flux(1:nel,3)+flux(eex(1:nel,3),1);
%
% nx= -1, ny=0  
    jmp(1:nel,4) = flux(1:nel,4)+flux(eex(1:nel,4),2);
%
%
  etime=toc;
%  fprintf('\nflux jump assembly took %9.4e seconds',etime) 
% 
% remove Dirichlet boundary edge contributions
      nbde=length(mbound(:,1));
      for k=1:nbde
         jmp(mbound(k,1),mbound(k,2))=0; 
      end
%
% Neumann natural outflow boundary condition correction ..
% This is often neglected, because it is going to zero weakly.
% neumanb=[] means that the Neumann edge is treated as an interior edge
if min(size(neumannb))>0    
   [jac,invjac,phi,dphidx,dphidy] = deriv(1,s,xl_v,yl_v);
   [psi,dpsidx,dpsidy] = qderiv(1,s,xl_v,yl_v); 
   fx_v=zero_v;
   for  ivtx=1:9 		 
	    fx_v = fx_v +( dpsidx(:,ivtx)).*invjac(:).*xsl_v(:,ivtx);
   end
   jmp(neumannb(:,1),2)=2*(fx_v(neumannb(:,1)));
end

return
