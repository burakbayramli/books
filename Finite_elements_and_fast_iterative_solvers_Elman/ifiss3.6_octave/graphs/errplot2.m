function errplot2(sol,eldata,mv,xy,x,y,fig)
%ERRPLOT2 plots Q2 solution and error on square domain
%   errplot2(sol,eldata,mv,xy,x,y,fig);
%   input
%          sol        nodal solution vector 
%          eldata     element error vector
%          mv         element mapping matrix
%          xy         vertex coordinate vector  
%          x          vector of x-axis interpolation points
%          y          vector of y-axis interpolation points
%          fig        figure number
%
%   IFISS function: DJS; 29 September 2013.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
fprintf('plotting Q2 solution and estimated errors... ')
% interpolate to a cartesian product mesh
[X,Y]=meshgrid(x,y);
xysol = griddata(xy(:,1),xy(:,2),sol,X,Y);
figure(fig)
subplot(221),contour(X,Y,xysol,20),axis('square')
axis('off'), squarex, title('Finite Element Solution','FontSize',12)
subplot(222),mesh(X,Y,xysol),axis('square')
view(330,30)
%%
nnxy=(length(x)-1)/2;
minx=min(x); maxx=max(x);
miny=min(y); maxy=max(y);
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
%---------------- corrected code
xcm=flipud(reshape(xc,nnxy,nnxy)');
ycm=flipud(reshape(yc,nnxy,nnxy)');
elm=flipud(reshape(eldata,nnxy,nnxy)');
subplot(223),contour(xcm,ycm,elm,15),axis('square')
title('Estimated Error','FontSize',12)
subplot(224),mesh(xcm,ycm,elm),axis('square')
view(330,30),
fprintf('done\n')
return
