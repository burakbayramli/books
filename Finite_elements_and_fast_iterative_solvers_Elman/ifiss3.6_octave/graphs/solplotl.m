function solplotl(sol,xy,x,y,fig)
%SOLPLOTL plots nodal data on L-shaped domain
%   solplotl(sol,xy,x,y,fig);
%   input
%          sol        nodal solution vector
%          xy         nodal coordinate vector  
%          x          vector of x-axis interpolation points
%          y          vector of y-axis interpolation points
%          fig        figure number
%
%   IFISS function: DJS; 30 March 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
fprintf('plotting solution... ')
% interpolate to a cartesian product mesh
[X,Y]=meshgrid(x,y);
xysol = griddata(xy(:,1),xy(:,2),sol,X,Y);
[II,JJ]=find(X<0 & Y<0); xysol(II,JJ)=nan;
figure(fig)
subplot(121),contour(X,Y,xysol,20),axis('square')
axis('off'), 
if all([min(x),max(x),min(y),max(y)] == [-1,1,-1,1]), ellx, end
subplot(122),mesh(X,Y,xysol),axis('square')
view(330,30)
fprintf('done\n')
return
