function wwwplotl(sol,eldata,mv,xy,x,y,fig)
%WWWPLOTL plots Q2 solution and error on L-shaped domain
%   wwwplotl(sol,eldata,mv,xy,x,y,fig)
%   input
%          sol        nodal solution vector 
%          eldata     element error vector
%          mv         element mapping matrix
%          xy         vertex coordinate vector  
%          x          vector of x-axis interpolation points
%          y          vector of y-axis interpolation points
%          fig        figure number
%
%   IFISS function: DJS; 3 November 2013.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
fprintf('plotting Q2 solution and estimated errors... ')
% interpolate to a cartesian product mesh
[X,Y]=meshgrid(x,y);
xysol = griddata(xy(:,1),xy(:,2),sol,X,Y);
[II,JJ]=find(X<0 & Y<0); xysol(II,JJ)=nan;
figure(fig)
subplot(121),contour(X,Y,xysol,20),axis('square')
axis('off'), ellx, title('Q2 Finite Element Solution','FontSize',12)
%%
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
%------------------ corrected code
% interpolate to a cartesian product mesh
x=0.5*(x(1:2:end-1)+x(3:2:end));
y=0.5*(y(1:2:end-1)+y(3:2:end));
[X,Y]=meshgrid(x,y);
xysol = griddata(xc,yc,eldata,X,Y);
[II,JJ]=find(X<0 & Y<0); xysol(II,JJ)=nan;
subplot(122),contour(X,Y,xysol,15),axis('square')
axis('off'), ellx, title('Estimated Error','FontSize',12)
fprintf('done\n')
return
