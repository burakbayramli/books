function [x,y,xy,xyp] = mg_diff_q2q1grid(x,y,xy,mv,mp,bound)
%MG_DIFF_Q2Q1GRID Q2-Q1 element grid generator for GMG
%   [x,y,xy,xyp] = mg_diff_q2q1grid(x,y,xy,mv,mp,bound);
%   input
%          x          x coordinate vector
%          y          y coordinate vector 
%          xy         nodal coordinate vector  
%          mv         Q2 macroelement mapping matrix
%          mp         Q1 element mapping matrix
%          bound      boundary vertex vector
%   output
%          xyp        vertex coordinate vector
%
%   IFISS function: HCE; 24 January 2004.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage

%   modelled after IFISS function q2q1grid, adapted for GMG
xx=xy(:,1); yy=xy(:,2); nvtx=length(xx);
%
%% recompute mid-side points in the case of stretched grids 
% y-direction
yv=yy; ny=length(y);
for k=2:2:ny;
   yold=y(k); ynew=0.5*(y(k+1)+y(k-1));
   l=find(yy==yold); yv(l)=ynew; y(k)=ynew;
end
% x-direction
xv=xx; nx=length(x);
for k=2:2:nx;
   xold=x(k); xnew=0.5*(x(k+1)+x(k-1));
   l=find(xx==xold); xv(l)=xnew; x(k)=xnew;
end
xy=[xv,yv];
%
mel=length(mv(:,1));
for k=1:mel
   for j=1:4
      map(mp(k,j))=mv(k,j);
   end
end
xyp=xy(map,:); 
