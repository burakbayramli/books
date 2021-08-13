function [Cp1,Cp2] = Cpre_q1q1(xy,ev)
%CPRE_Q1Q1 generate stabilization matrices for Q1-Q1 
%   [Cp1,Cp2] = Cpre_q1q1(xy,ev);
%   input
%          xy         Q2 nodal coordinate vector 
%          ev         element mapping matrix
%   output
%          Cp1        pressure stabilization 1 for preconditioner
%          Cp2        pressure stabilization 2 for preconditioner
%   IFISS function: HCE; 7 July 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 

nngpt=4; 
x=xy(:,1); y=xy(:,2);
xp=xy(:,1); yp=xy(:,2);
nvtx=length(x); nu=2*nvtx; np=length(xp); 
nel=length(ev(:,1)); 
%
% initialise global matrices
Cp1 = sparse(np,np);
Cp2 = sparse(np,np);
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
end

% 
% element assembly into global matrices
cpe1 = zeros(nel,4,4);
cpe2 = zeros(nel,4,4);
 
% loop over Gauss points
for igpt = 1:nngpt
   sigpt=s(igpt);
   tigpt=t(igpt);
   wght=wt(igpt);
   [jac,invjac,phi,dphidx,dphidy] = deriv(sigpt,tigpt,xl_v,yl_v);
   for j = 1:4
      for i = 1:4
          cpe1(:,i,j) = cpe1(:,i,j) + .25 * wght*(phi(:,i)-0.25) .*(phi(:,j)-0.25) ;
          cpe2(:,i,j) = cpe2(:,i,j) +  (16*jac(:)) .\ (wght*(phi(:,i)-0.25) .*(phi(:,j)-0.25));
      end
   end
% end of Gauss point loop
end  

%%  element assembly into global matrices
for krow=1:4
   nrow=ev(:,krow);	 
   for kcol=1:4
      ncol=ev(:,kcol);	  
      Cp1 = Cp1 + sparse(nrow,ncol,cpe1(:,krow,kcol),nvtx,nvtx);
      Cp2 = Cp2 + sparse(nrow,ncol,cpe2(:,krow,kcol),nvtx,nvtx);
   end
end
