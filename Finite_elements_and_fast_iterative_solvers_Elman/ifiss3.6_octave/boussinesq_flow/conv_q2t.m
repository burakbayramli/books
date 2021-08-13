function Cv=conv_q2t(xyt,mt2,u)
%CONV_Q2T  computes Q2 temperature convection matrix 
%   Cv=conv_q2t(xyt,mt2,u);
%
%   input
%          xyt        Q2 nodal coordinate vector  
%          mt2        Q2 element mapping matrix
%          u          wind velocity defined at the nodes
%   output
%          Cv         Q2 temperature convection matrix
%   IFISS function: DJS; 27 April 2012.
% Copyright (c) 2012 D.J. Silvester, M.D. Mihajlovic.
nngpt=9; 
x=xyt(:,1); y=xyt(:,2);
nvtx=length(x); nel=length(mt2(:,1));
nv=2*nvtx;
ux=u(1:nvtx); uy=u(nvtx+1:2*nvtx);
Cv = sparse(nvtx,nvtx);
%
%% Definition of Gauss quadrature rules
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
%  Inner loop over elements  
for ivtx=1:4
   xl_v(:,ivtx)=x(mt2(:,ivtx));
   yl_v(:,ivtx)=y(mt2(:,ivtx)); 
end
for idx=1:9	   
   xsl(:,idx)=ux(mt2(:,idx),1);
   ysl(:,idx)=uy(mt2(:,idx),1);
end
Cve=zeros(nel,9,9);  %  convection element matrices
% 
%  Loop over Gauss points
for igpt = 1:nngpt
   sigpt=s(igpt);
   tigpt=t(igpt);
   wght=wt(igpt);
%   
%  Evaluation of derivatives etc
   [jac,invjac,phi,dphidx,dphidy]=deriv(sigpt,tigpt,xl_v,yl_v);
   [psi,dpsidx,dpsidy]=qderiv(sigpt,tigpt,xl_v,yl_v); 
   u_x=zeros(nel,1); u_y=zeros(nel,1);
   for k=1:9   %  interpolate of the velocity over the element
	  u_x(:)=u_x(:)+xsl(:,k).*psi(:,k);
      u_y(:)=u_y(:)+ysl(:,k).*psi(:,k);	 
   end
   for j=1:9
      for i=1:9               
		 Cve(:,i,j)=Cve(:,i,j)+wght*u_x(:).*psi(:,i).*dpsidx(:,j);
         Cve(:,i,j)=Cve(:,i,j)+wght*u_y(:).*psi(:,i).*dpsidy(:,j);
      end
   end
end     % end of Gauss point loop
%
%  Assembly of local matrices into global matrix
for krow=1:9
   nrow=mt2(:,krow);	 
   for kcol=1:9
      ncol=mt2(:,kcol);	  
      Cv=Cv+sparse(nrow,ncol,Cve(:,krow,kcol),nvtx,nvtx);
   end
end
return
