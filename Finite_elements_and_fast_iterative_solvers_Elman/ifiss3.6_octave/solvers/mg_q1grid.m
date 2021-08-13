function [ev,ebound]=mg_q1grid(x,y,xy,mv,bound,mbound)
%MG_Q1GRID bilinear element grid generator for GMG
%   [ev,ebound]=mg_q1grid(x,y,xy,mv,bound,mbound)
%   input
%          x          x-grid coordinates
%          y          y-grid coordinates
%          xy         vertex coordinate vector  
%          mv         Q2 macroelement mapping matrix
%          bound      boundary vertex vector
%          mbound     macroelement boundary edge vector
%   output
%          ev         element mapping matrix
%          ebound     element boundary mapping matrix
%
%   IFISS function: AR; 19 November 2001
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage

xx=xy(:,1); yy=xy(:,2); nvtx=length(xx);
adj=sparse(nvtx,nvtx);
mel=length(mv(:,1)); nel=4*mel;
ev=zeros(nel,4);
%
%% loop over macroelements
for k=1:mel
% first element
ke=4*k-3;
ev(ke,1)=mv(k,1);
ev(ke,2)=mv(k,5);
ev(ke,3)=mv(k,9);
ev(ke,4)=mv(k,8);
% second element
ke=4*k-2;
ev(ke,1)=mv(k,5);
ev(ke,2)=mv(k,2);
ev(ke,3)=mv(k,6);
ev(ke,4)=mv(k,9);
% third element
ke=4*k-1;
ev(ke,1)=mv(k,9);
ev(ke,2)=mv(k,6);
ev(ke,3)=mv(k,3);
ev(ke,4)=mv(k,7);
% fourth element
ke=4*k;
ev(ke,1)=mv(k,8);
ev(ke,2)=mv(k,9);
ev(ke,3)=mv(k,7);
ev(ke,4)=mv(k,4);
end
%
%% define element edges
ect=1;
% bottom boundary edges
k1=find(mbound(:,2)==1)';
for k=mbound(k1)
   ebound(ect,1)=4*k-3; ebound(ect+1,1)=4*k-2; 
   ebound(ect,2)=1    ; ebound(ect+1,2)=1;
   ect=ect+2;
end
% right boundary edges
k2=find(mbound(:,2)==2)';
for k=mbound(k2)
   ebound(ect,1)=4*k-2; ebound(ect+1,1)=4*k-1; 
   ebound(ect,2)=2    ; ebound(ect+1,2)=2;
   ect=ect+2;
end
% top boundary edges
k3=find(mbound(:,2)==3)';
for k=mbound(k3)
   ebound(ect,1)=4*k-1; ebound(ect+1,1)=4*k; 
   ebound(ect,2)=3    ; ebound(ect+1,2)=3;
   ect=ect+2;
end
% left boundary edges
k4=find(mbound(:,2)==4)';
for k=mbound(k4)
   ebound(ect,1)=4*k; ebound(ect+1,1)=4*k-3; 
   ebound(ect,2)=4    ; ebound(ect+1,2)=4;
   ect=ect+2;
end
return
