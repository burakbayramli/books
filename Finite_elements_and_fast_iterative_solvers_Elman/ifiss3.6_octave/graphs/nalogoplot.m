function nalogoplot(eldata,mv,xy,x,y)
%NALOGOPLOT 
%   nalogoplot(error_tot,mv,xy,x,y);
%   input
%          eldata     element data vector
%          mv         element mapping matrix
%          xy         nodal coordinate vector  
%          x          vector of x-axis node points
%          y          vector of y-axis node points
%
%   IFISS function: DJS; 20 October 2013.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
nnxy=(length(x)-1)/2;
minx=min(x); maxx=max(x);
miny=min(y); maxy=max(y);
figure(101)
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
%---------------- corrected code
xcm=flipud(reshape(xc,nnxy,nnxy)');
ycm=flipud(reshape(yc,nnxy,nnxy)');
elm=flipud(reshape(eldata,nnxy,nnxy)');
contour(xcm,ycm,elm,15,'LineWidth',2),axis('square')
title('Hello','FontSize',14)
axis off
return