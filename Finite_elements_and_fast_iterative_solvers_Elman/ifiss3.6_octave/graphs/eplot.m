function eplot(eldata,ev,xy,x,y,fig,titlestr)
%EPLOT plots Q1 element data on arbitrary domain
%   eplot(eldata,ev,xy,x,y,fig,'Element data');
%   input
%          eldata     element data vector
%          ev         element mapping matrix
%          xy         nodal coordinate vector  
%          x          vector of x-axis interpolation points
%          y          vector of y-axis interpolation points
%          fig        figure number
%          titlestr   character string
%
%   IFISS function: DJS; 23 November 2014.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
fprintf('plotting element data... ')
figure(fig)
xx=xy(:,1); yy=xy(:,2);
xmin=min(x); xmax=max(x);
ymin=min(y); ymax=max(y);
nel=length(eldata);
xc = zeros(nel,2);
% loop over elements    
for ielem = 1:nel
   xl = xx(ev(ielem,1:4));
   yl = yy(ev(ielem,1:4));
   xc(ielem,1) = 0.25*sum(xl);
   xc(ielem,2) = 0.25*sum(yl);
end
%
% interpolate to a cartesian product mesh
x=0.5*(x(1:end-1)+x(2:end));
y=0.5*(y(1:end-1)+y(2:end));
[X,Y]=meshgrid(x,y);
xysol = griddata(xc(:,1),xc(:,2),eldata,X,Y);
subplot(121),contour(X,Y,xysol,15),axis('square')
title(titlestr,'FontSize',12)
subplot(122),mesh(X,Y,xysol),axis('square')
axis([xmin,xmax,ymin,ymax])
view(330,30)
fprintf('done\n')
return
