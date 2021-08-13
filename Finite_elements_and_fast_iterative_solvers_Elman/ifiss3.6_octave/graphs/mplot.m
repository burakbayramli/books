function mplot(eldata,ev,xy,x,y,fig)
%MPLOT plots 2x2 macro data on square domain
%   mplot(eldata,ev,xy,x,y,fig);
%   input
%          eldata     element data vector
%          ev         element mapping matrix
%          xy         nodal coordinate vector  
%          x          vector of x-axis interpolation points
%          y          vector of y-axis interpolation points
%          fig        figure number
%
%   IFISS function: DJS; 30 April 2012.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
fprintf('plotting 2x2 element data... ')
figure(fig)
xx=xy(:,1); yy=xy(:,2);
nel=length(eldata);
mxx=0.25*(xx(ev(:,1))+xx(ev(:,2))+xx(ev(:,3))+xx(ev(:,4)));
myy=0.25*(yy(ev(:,1))+yy(ev(:,2))+yy(ev(:,3))+yy(ev(:,4)));
meldata = zeros(nel/4,1);
xc = zeros(nel/4,2);
% loop over elements    
for imel = 1:nel/4
	meldata(imel)=mean(eldata(4*imel-3:4*imel));
    xc(imel,1)=mean(mxx(4*imel-3:4*imel));	
	xc(imel,2)=mean(myy(4*imel-3:4*imel));	
end
%
% interpolate to a cartesian product mesh
x=x(2:2:end);%x=0.5*(x(1:end-1)+x(2:end));
y=y(2:2:end);%y=0.5*(y(1:end-1)+y(2:end));
[X,Y]=meshgrid(x,y);
xysol = griddata(xc(:,1),xc(:,2),meldata,X,Y);
subplot(121),contour(X,Y,xysol,15),axis('square')
title('Contours of estimated error','FontSize',12);
subplot(122),mesh(X,Y,xysol),axis('square')
title('Estimated error','FontSize',12);
view(330,30)
fprintf('done\n')
return
