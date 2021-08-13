function [x,y,xy,xyp,mp,map] = q2q1gridx(x,y,xy,mv,bound);
%Q2Q1GRIDX Q2-Q1 element grid generator
%   [x,y,xy,xyp,mp,map] = q2q1gridx(x,y,xy,mv,bound);
%   input
%          x          x coordinate vector
%          y          y coordinate vector 
%          xy         nodal coordinate vector  
%          mv         Q2 macroelement mapping matrix
%          bound      boundary vertex vector
%   output
%          xyp        vertex coordinate vector
%          mp         Q1 element mapping matrix
%          map        Q2->Q1 vertex mapping vector 
%
%
%   IFISS function: DJS; 22 October 2009, 1 May 2012.
% Copyright (c) 2006 D.J. Silvester, H.C. Elman, A. Ramage 
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
% compute vertex mapping vector mp 
mel=length(mv(:,1));
mmv=reshape(mv(:,1:4),1,4*mel);
map=unique(sort(mmv));
mmp=zeros(1,4*mel); for jj=1:4*mel, mmp(jj)=find(map==mmv(jj)); end
mp=reshape(mmp,mel,4);
xyp=xy(map,:);
%
% plotting of the grid 
adj=sparse(nvtx,nvtx);
for i=1:mel
adj(mv(i,1),mv(i,2))=1;
adj(mv(i,2),mv(i,3))=1;
adj(mv(i,3),mv(i,4))=1;
adj(mv(i,4),mv(i,1))=1;
end
figure(30)
gplot(adj,xy,'b');
axis('square')
hold on
plot(xy(:,1),xy(:,2),'ro')
xybd=xy(bound,:);
plot(xybd(:,1),xybd(:,2),'ko')
plot(xyp(:,1),xyp(:,2),'rx')
hold off
title('Q2-Q1 finite element subdivision')
drawnow, pause(5), 
figure(30),set(gcf,'Visible','off'), drawnow
return
