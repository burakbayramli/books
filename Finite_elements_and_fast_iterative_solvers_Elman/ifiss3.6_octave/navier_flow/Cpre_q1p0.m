function [Cp1,Cp2] = Cpre_q1p0(xy,xyp,ev,domain)
%CPRE_Q1P0 generate stabilization matrices for Q1-P0 
%   [Cp1,Cp2] = Cpre_q1p0(xy,xyp,ev,domain);
%   input
%          xy         Q2 nodal coordinate vector 
%          xyp        Q1 nodal coordinate vector  
%          ev         element mapping matrix
%          domain     domain specifier
%   output
%          Cp1        pressure stabilization 1 for preconditioner
%          Cp2        pressure stabilization 2 for preconditioner
%   IFISS function: HCE; 27 May 2012.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 

nngpt=4; 
x=xy(:,1); y=xy(:,2);
xp=xyp(:,1); yp=xyp(:,2);
nvtx=length(x); nu=2*nvtx; np=length(xp); 
nel=length(ev(:,1)); mp=(1:nel)';
lx=max(x)-min(x); ly=max(y)-min(y);
hx=max(diff(x)); hy=max(diff(y));

% Hack to get access to Q2 element mapping mv
if     domain==1, load cavity_grid.mat mv
elseif domain==3, load step_grid.mat   mv
elseif domain==4, load obstacle_grid.mat mv
else   error('Stabilized LSC preconditioner not defined for this domain.');
end

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

% inner loop over elements
for ivtx = 1:4
   xl_v(:,ivtx) = x(ev(:,ivtx));
   yl_v(:,ivtx) = y(ev(:,ivtx)); 
end

mpe = zeros(nel,1,1);
% loop over Gauss points
for igpt = 1:nngpt
   sigpt=s(igpt);
   tigpt=t(igpt);
   wght=wt(igpt);
   [jac,invjac,phi,dphidx,dphidy] = deriv(sigpt,tigpt,xl_v,yl_v);
   mpe(:,1,1) = mpe(:,1,1) + wght*jac(:);
% end of Gauss point loop
end  

% pressure mass matrix
m = sparse(mp,mp,mpe(:,1,1),np,np);

% stabilisation matrix
mel=length(mv(:,1));
hm=zeros(mel,1);

elarea=full(diag(m));
hm(:)=sum(reshape(elarea,4,mel))';
hm=hm/4; 
%      
%      cm =  [ le41+le12,     -le12,         0,     -le41;
%  	               -le12, le12+le23,     -le23,         0;
%                      0,     -le23, le23+le34,     -le34;
%	 			    -le41,         0,     -le34, le34+le41];
%
cm=zeros(mel,4,4); 
le12=hm~=0; 
cm(:,1,1) =cm(:,1,1)+ le12; cm(:,1,2)=-le12;
cm(:,2,2) =cm(:,2,2)+ le12; cm(:,2,1)=-le12;
le23=hm~=0;
cm(:,2,2) =cm(:,2,2)+ le23; cm(:,2,3)=-le23;
cm(:,3,3) =cm(:,3,3)+ le23; cm(:,3,2)=-le23;
le34=hm~=0;
cm(:,3,3) =cm(:,3,3)+ le34; cm(:,3,4)=-le34;
cm(:,4,4) =cm(:,4,4)+ le34; cm(:,4,3)=-le34;
le41=hm~=0;
cm(:,4,4) =cm(:,4,4)+ le41; cm(:,4,1)=-le41;
cm(:,1,1) =cm(:,1,1)+ le41; cm(:,1,4)=-le41;	  
%
%  macroelement assembly into global matrices
Cp1=sparse(np,np);
for krow=1:4
   nrow=(0:4:nel-4)+krow; 
   for kcol=1:4
      ncol=(0:4:nel-4)+kcol;	 
      Cp1 = Cp1 + sparse(nrow,ncol,cm(:,krow,kcol),np,np);
   end
end

cm=zeros(mel,4,4); 
le12=1./hm; 
cm(:,1,1) =cm(:,1,1)+ le12; cm(:,1,2)=-le12;
cm(:,2,2) =cm(:,2,2)+ le12; cm(:,2,1)=-le12;
le23=1./hm;
cm(:,2,2) =cm(:,2,2)+ le23; cm(:,2,3)=-le23;
cm(:,3,3) =cm(:,3,3)+ le23; cm(:,3,2)=-le23;
le34=1./hm;
cm(:,3,3) =cm(:,3,3)+ le34; cm(:,3,4)=-le34;
cm(:,4,4) =cm(:,4,4)+ le34; cm(:,4,3)=-le34;
le41=1./hm;
cm(:,4,4) =cm(:,4,4)+ le41; cm(:,4,1)=-le41;
cm(:,1,1) =cm(:,1,1)+ le41; cm(:,1,4)=-le41;

%  macroelement assembly into global matrices
Cp2=sparse(np,np);
for krow=1:4
   nrow=(0:4:nel-4)+krow; 
   for kcol=1:4
      ncol=(0:4:nel-4)+kcol;	 
      Cp2 = Cp2 + sparse(nrow,ncol,cm(:,krow,kcol),np,np);
   end
end
