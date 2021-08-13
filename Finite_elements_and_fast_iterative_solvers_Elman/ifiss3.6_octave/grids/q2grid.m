function [x,y,xy] = q2grid(x,y,xy,mv,bound);
%Q2GRID biquadratic element grid generator 
%   [x,y,xy] = q2grid(x,y,xy,mv,bound);
%   input
%          x          x coordinate vector
%          y          y coordinate vector 
%          xy         vertex coordinate vector  
%          mv         Q2 macroelement mapping matrix
%          bound      boundary vertex vector
%
% postpocesses Q2 element partitioning information to
% give standard approximation in the case of stretched grids
%   IFISS function: DJS; 28 February 2005, 5 January 2011.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
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
% plotting of the grid 
mel=length(mv(:,1));
%if mel <= 256,
adj=sparse(nvtx,nvtx);
for i=1:mel
adj(mv(i,1),mv(i,2))=1;
adj(mv(i,2),mv(i,3))=1;
adj(mv(i,3),mv(i,4))=1;
adj(mv(i,4),mv(i,1))=1;
end
figure(10)
gplot(adj,xy,'b');
axis('square')
hold on
plot(xy(:,1),xy(:,2),'ro')
xybd=xy(bound,:);
plot(xybd(:,1),xybd(:,2),'ko')
hold off
title('Q2 finite element subdivision')
drawnow
%end
return
