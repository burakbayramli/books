function eplotl2(eldata,mv,xy,x,y,fig,titlestr)
%EPLOTL2 plots Q2 element data on L-shaped domain
%   eplotl2(eldata,mv,xy,x,y,fig);
%   input
%          eldata     element data vector
%          ev         element mapping matrix
%          xy         nodal coordinate vector  
%          x          vector of x-axis node points
%          y          vector of y-axis node points
%          fig        figure number
%          titlestr   character string
%
%   IFISS function: DJS; 29 September 2013.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
fprintf('plotting Q2 element data... ')
minx=min(x); maxx=max(x);
miny=min(y); maxy=max(y);
figure(fig)
xx=xy(:,1); yy=xy(:,2);
nel=length(eldata);
xc=zeros(nel,1); yc=xc;
% loop over elements    
for ielem = 1:nel
   xl = xx(mv(ielem,1:4));
   yl = yy(mv(ielem,1:4));
   xc(ielem) = 0.25*sum(xl);
   yc(ielem) = 0.25*sum(yl);
end
%
%------------------ corrected code
% interpolate to a cartesian product mesh
x=0.5*(x(1:2:end-1)+x(3:2:end));
y=0.5*(y(1:2:end-1)+y(3:2:end));
[X,Y]=meshgrid(x,y);
xysol = griddata(xc,yc,eldata,X,Y);
[II,JJ]=find(X<0 & Y<0); xysol(II,JJ)=nan;
subplot(121),contour(X,Y,xysol,15),axis('square')
title(titlestr,'FontSize',12)
subplot(122),mesh(X,Y,xysol),axis('square')
view(330,30)     
V=axis; axis([-1,V(2:6)])
fprintf('done\n')
return
