function eplot2(eldata,mv,xy,x,y,fig,titlestr)
%EPLOT2 plots Q2 element data on square-shaped domain
%   eplot2(eldata,mv,xy,x,y,fig,'Element data');
%   input
%          eldata     element data vector
%          mv         element mapping matrix
%          xy         nodal coordinate vector  
%          x          vector of x-axis node points
%          y          vector of y-axis node points
%          fig        figure number
%          titlestr   character string
%
%   IFISS function: DJS; 29 September 2013.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
fprintf('plotting Q2 element data... ')
nnxy=(length(x)-1)/2;
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
%---------------- corrected code
xcm=flipud(reshape(xc,nnxy,nnxy)');
ycm=flipud(reshape(yc,nnxy,nnxy)');
elm=flipud(reshape(eldata,nnxy,nnxy)');
subplot(121),contour(xcm,ycm,elm,15),axis('square')
title(titlestr,'FontSize',12)
subplot(122),mesh(xcm,ycm,elm),axis('square')
view(330,30),
fprintf('done\n')
return