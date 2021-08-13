function [Av,Qv,B,Bx,By,Qp,M,At,Qt,BBx,BBy,f,g,h]=bouss_q2q1q2(xyv,xyp,...
                                                  xyt,mv2,mp1,mt2,nngpt,lh)
%BOUSS_Q2Q1Q2 Q2-Q1-Q2 matrix generation and assembly
% [Av,Qv,B,Bx,By,Qp,M,At,Qt,BBx,BBy,f,g,h] = bouss_q2q1q2(xyv,xyp,xyt,...
%                                                   mv2,mp1,mt2,nngpt,lh);
%   input
%          xyv        Q2 nodal coordinate vector 
%          xyp        Q1 nodal coordinate vector  
%          xyt        Q1 nodal coordinate vector  
%          mv2        Q2 element mapping matrix
%          mp1        Q1 element mapping matrix
%          mt2        Q2 element mapping matrix
%          nngpt      number of Gauss points
%          lh         level: set to 1 for top level 
%   output
%          Av         Q2 vector diffusion matrix
%          Qv         Q2 vector mass matrix
%          B          Q2-Q1 divergence matrix 
%          Bx         Q2 x-derivative matrix    
%          By         Q2 y-derivative matrix    
%          Qp         Q1 mass matrix 
%          M          Q2 vector mass matrix 
%          At         Q2 scalar diffusion matrix
%          Qt         Q2 scalar mass matrix
%          BBx        Q1 x-derivative matrix    
%          BBy        Q1 y-derivative matrix    
%          f          velocity rhs vector
%          g          pressure rhs vector
%          h          temperature rhs vector
%
%   No boundary conditions are applied.
%   IFISS function: DJS; 27 April 2012.
% Copyright (c) 2012 D.J. Silvester, M.D. Mihajlovic.

%
tic;            
xv=xyv(:,1);            %  x coordinates of velocity nodes
yv=xyv(:,2);            %  y coordinates of velocity nodes
xp=xyp(:,1);            %  x coordinates of pressure nodes 
yp=xyp(:,2);            %  y coordinates of pressure nodes
xt=xyt(:,1);            %  x coordinates of temperature nodes
yt=xyt(:,2);            %  y coordinates of temperature nodes
nvtx=length(xv);        %  number of velocity dofs (int+bnd)
nv=2*nvtx;              %  dimension of A,Q
np=length(xp);          %  number of pressure dofs
nt=length(xt);          %  number of temperature dofs
nel=length(mv2(:,1));   %  number of elements
fprintf('   Number of elements:            %6i\n',nel); 
fprintf('   Number of velocity dofs (x+y): %6i\n',nv);
fprintf('   Number of pressure dofs:       %6i\n',np);
fprintf('   Number of temperature dofs:    %6i\n',nt); 
fprintf('   Total number of dofs:          %6i\n',nv+np+nt);
%
%% Initialisation of the global matrices
%  
Av=sparse(nv,nv);        %  velocity diffusion matrix
Qv=sparse(nv,nv);        %  velocity mass matrix
Bx=sparse(np,nvtx);      %  x component of div matrix
By=sparse(np,nvtx);      %  y component of div matrix
B=sparse(np,nv);         %  divergence matrix
At=sparse(nt,nt);        %  temperature diffusion matrix
Qt=sparse(nt,nt);        %  temperature mass matrix  
if(lh==1)   %  finest grid level
   Qp=sparse(np,np);        %  pressure mass matrix
   BBx=sparse(nvtx,nvtx);   %  x comp. of vel. div matrix
   BBy=sparse(nvtx,nvtx);   %  y comp. of vel. div matrix 
   Mx=sparse(nvtx,nt);      %  x-component coupling (NS-CD) mass matrix
   My=sparse(nvtx,nt);      %  y-component coupling (NS-CD) mass matrix
   M=sparse(nv,nt);         %  coupling (NS-CD) mass matrix 
   f=zeros(nv,1);           %  velocity rhs vector
   g=zeros(np,1);           %  pressure rhs vector
   h=zeros(nt,1);           %  temperature rhs vector
else   %  coarser grid levels
   error('multiple levels not implemented!')
end
%
%% Definition of Gauss quadrature rules
%
switch(nngpt)
   case(4)          %  2x2 Gauss point rule
      gpt=1.0e0/sqrt(3.0e0);
      s=[-gpt,gpt,gpt,-gpt];      %  s-coord of nodes
	  t=[-gpt,-gpt,gpt,gpt];      %  t-coord of nodes
	  wt=[1,1,1,1];               %  weights
   case(9)       %  3x3 Gauss point rule
	  gpt=sqrt(0.6);
	  w1=25/81; w2=40/81; w3=64/81;
	  s=[-gpt,gpt,gpt,-gpt,0.0,gpt,0.0,-gpt,0.0];   %  s-coord of nodes
	  t=[-gpt,-gpt,gpt,gpt,-gpt,0.0,gpt,0.0,0.0];   %  t-coord of nodes
	  wt=[w1,w1,w1,w1,w2,w2,w2,w2,w3];              %  weights
   otherwise
	  error('Check Gauss point integration specification')
end
%
%%  Getting the element vertices 
%
for ivtx=1:4
   xl_v(:,ivtx)=xv(mv2(:,ivtx));  %  x coordinates
   yl_v(:,ivtx)=yv(mv2(:,ivtx));  %  y coordinates
end
%
%% Initialisation of the elemental matrices
%
Ave=zeros(nel,9,9);     %  velocity diffusion element matrices
Qve=zeros(nel,9,9);     %  velocity mass element matrices
Bxe=zeros(nel,4,9);     %  x-divergence element matrices
Bye=zeros(nel,4,9);     %  y-divergence element matrices
Ate=zeros(nel,9,9);     %  temperature diffusion element matrices
Qte=zeros(nel,9,9);     %  temperature mass element matrices 
if(lh==1)   %  finest grid level
   Qpe=zeros(nel,4,4);     %  pressure mass element matrices
   BBxe=zeros(nel,9,9);    %  x-vel. div element matrices  
   BBye=zeros(nel,9,9);    %  y-vel. div element matrices
   Mxe=zeros(nel,9,9);     %  x-coupling mass element matrices
   Mye=zeros(nel,9,9);     %  y-coupling mass element matrices  
   fe=zeros(nel,9);        %  velocity element rhs vector
   ge=zeros(nel,4);        %  pressure element rhs vector
   he=zeros(nel,9);        %  temperature element rhs vector
end
%
%%  Loop over Gauss quadrature points
%
for igpt=1:nngpt 
   sigpt=s(igpt); tigpt=t(igpt); wght=wt(igpt);
%
%%  Evaluate derivatives etc.
%
   [jac,invjac,phi,dphidx,dphidy]=deriv(sigpt,tigpt,xl_v,yl_v);
   [psi,dpsidx,dpsidy]=qderiv(sigpt,tigpt,xl_v,yl_v);
%   [grhs]=gauss_force(sigpt,tigpt,xl_v,yl_v);
%
%%  Loops over the nodes
%
   for j=1:9
      for i=1:9
         Ave(:,i,j)=Ave(:,i,j)+wght*dpsidx(:,i).*dpsidx(:,j).*invjac(:);
	     Ave(:,i,j)=Ave(:,i,j)+wght*dpsidy(:,i).*dpsidy(:,j).*invjac(:);
         Qve(:,i,j)=Qve(:,i,j)+wght*psi(:,i).*psi(:,j).*jac(:);
	     Ate(:,i,j)=Ate(:,i,j)+wght*dpsidx(:,i).*dpsidx(:,j).*invjac(:);
	     Ate(:,i,j)=Ate(:,i,j)+wght*dpsidy(:,i).*dpsidy(:,j).*invjac(:);
         Qte(:,i,j)=Qte(:,i,j)+wght*psi(:,i).*psi(:,j).*jac(:);
         if(lh==1)   %  finest grid level
            BBxe(:,i,j)=BBxe(:,i,j)-wght*psi(:,i).*dpsidx(:,j);
	        BBye(:,i,j)=BBye(:,i,j)-wght*psi(:,i).*dpsidy(:,j);
            Mxe(:,i,j)=0.0;
	        Mye(:,i,j)=Mye(:,i,j)+wght*psi(:,i).*psi(:,j).*jac(:);
         end
      end
      if(lh==1)   %  finest grid level
         fe(:,j)=0.0;
         he(:,j)=0.0;
      end
      for i=1:4
         Bxe(:,i,j)=Bxe(:,i,j)-wght*phi(:,i).*dpsidx(:,j);
	     Bye(:,i,j)=Bye(:,i,j)-wght*phi(:,i).*dpsidy(:,j);
      end
   end
   if(lh==1)   %  finest grid level
      for j=1:4
         for i=1:4
            Qpe(:,i,j)=Qpe(:,i,j)+wght*phi(:,i).*phi(:,j).*jac(:);     
         end
         ge(:,j)=0.0;  % ge(:,j)+wght*grhs(:).*phi(:,j).*jac(:);
      end  
   end
end    %  Gauss point loop   	   
%       
%%  Assembly of element matrices into the global ones	
%
%   Velocity components matrices
%
for krow=1:9
   nrow=mv2(:,krow);
   for kcol=1:9
	  ncol=mv2(:,kcol); 
	  Av=Av+sparse(nrow,ncol,Ave(:,krow,kcol),nv,nv);           %  (1,1) block
	  Av=Av+sparse(nrow+nvtx,ncol+nvtx,Ave(:,krow,kcol),nv,nv); %  (2,2) block
      if(lh==1)   %  finest grid level
	     BBx=BBx+sparse(nrow,ncol,BBxe(:,krow,kcol),nvtx,nvtx);
	     BBy=BBy+sparse(nrow,ncol,BBye(:,krow,kcol),nvtx,nvtx);
      end
	  Qv=Qv+sparse(nrow,ncol,Qve(:,krow,kcol),nv,nv);           %  (1,1) block
	  Qv=Qv+sparse(nrow+nvtx,ncol+nvtx,Qve(:,krow,kcol),nv,nv); %  (2,2) block
      if(lh==1)   %  finest grid level
         ncol=mt2(:,kcol);
         My=My+sparse(nrow,ncol,Mye(:,krow,kcol),nvtx,nt);
      end
   end
   if(lh==1)   %  finest grid level
      f(nrow,1)=f(nrow,1)+fe(:,krow);
   end
   for kcol=1:4
	  ncol=mp1(:,kcol); 
	  Bx=Bx+sparse(ncol,nrow,Bxe(:,kcol,krow),np,nvtx);
	  By=By+sparse(ncol,nrow,Bye(:,kcol,krow),np,nvtx);         
   end
end	        
%
%   Divergence matrix
%
B=[Bx,By];
%
%   Pressure matrices
%
if(lh==1)   %  finest grid level
   for krow=1:4
      nrow=mp1(:,krow);
      for kcol=1:4
	     ncol=mp1(:,kcol);
	     Qp=Qp+sparse(nrow,ncol,Qpe(:,krow,kcol),np,np);
      end
      g(nrow,1)=g(nrow,1)+ge(:,krow);
   end
end
%
%  Temperature matrices
%
for krow=1:9
   nrow=mt2(:,krow);
   for kcol=1:9
	  ncol=mt2(:,kcol);
	  At=At+sparse(nrow,ncol,Ate(:,krow,kcol),nt,nt);
      Qt=Qt+sparse(nrow,ncol,Qte(:,krow,kcol),nt,nt);
   end
   if(lh==1)   %  finest grid level
      h(nrow,1)=h(nrow,1)+he(:,krow);
   end
end	            
%
%  Coupling mass matrix
%
if(lh==1)   %  finest grid level
   M=[Mx',My']';
end
etoc=toc; 
fprintf('   System setup in %8.3f seconds\n',etoc); 
return
