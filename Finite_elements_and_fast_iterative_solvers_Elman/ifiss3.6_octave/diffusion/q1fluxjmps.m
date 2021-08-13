function jmp = q1fluxjmps(q1sol,eex,xy,ev,ebound,s)
%Q1FLUXJMPS vectorised flux jumps for rectangular Q1 grid 
%   jmp = q1fluxjmps(q1sol,eex,xy,ev,ebound,s);
%   input
%          q1sol        vertex solution vector
%          eex          presorted element connectivity array 
%          xy           vertex coordinate vector  
%          ev           element mapping matrix
%          ebound       element edge boundary matrix 
%          s            quadrature point coordinate in (-1,1) 
%   output 
%          jmp          component elementwise edge flux jumps
%
%   IFISS function: DJS; 27 September 2013.
% Copyright (c) 2010 D.J. Silvester, Qifeng Liao 
tic
x=xy(:,1); y=xy(:,2); nvtx=length(x);
nel=length(ev(:,1)); 
      
% initialise global matrices
      jmp   = zeros(nel,4);
      flux  = zeros(nel,4);
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
% nx= 0, ny=-1    
    [jac,invjac,phi,dphidx,dphidy] = deriv(s,-1,xl_v,yl_v);
    fx_v=zero_v;  
    for  ivtx=1:4 		 
	   fx_v = fx_v +(-dphidy(:,ivtx)).*invjac(:).*sl_v(:,ivtx);
    end
    flux(:,1)=fx_v;
	 
%        
% nx= 1, ny= 0
    [jac,invjac,phi,dphidx,dphidy] = deriv(1,s,xl_v,yl_v);
    fx_v=zero_v; 
    for  ivtx=1:4 		 
	   fx_v = fx_v +( dphidx(:,ivtx)).*invjac(:).*sl_v(:,ivtx);
    end
    flux(:,2)=fx_v;
%        
% nx= 0, ny= 1
    [jac,invjac,phi,dphidx,dphidy] = deriv(s,1,xl_v,yl_v);
    fx_v=zero_v; 
    for  ivtx=1:4 		 
	   fx_v = fx_v +( dphidy(:,ivtx)).*invjac(:).*sl_v(:,ivtx);
    end  
    flux(:,3)=fx_v;  
% 
% nx=-1, ny= 0
    [jac,invjac,phi,dphidx,dphidy] = deriv(-1,s,xl_v,yl_v); 
    fx_v=zero_v; 
    for  ivtx=1:4 		 
	   fx_v = fx_v +(-dphidx(:,ivtx)).*invjac(:).*sl_v(:,ivtx);
    end  
    flux(:,4)=fx_v; 

      
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
%  fprintf('flux jump assembly took %6.3e seconds\n',etime)
% 
% remove Dirichlet boundary edge contributions
      nbde=length(ebound(:,1));
      for k=1:nbde
         jmp(ebound(k,1),ebound(k,2))=0; 
      end
%
return
