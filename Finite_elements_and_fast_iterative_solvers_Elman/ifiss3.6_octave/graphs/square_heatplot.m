function square_heatplot(sol,tt,xy,x,y,ftime,fig)
%SQUARE_HEATPLOT evolves temperature data on square domain
%   square_heatplot(sol,tt,xy,x,y,ftime,fig)
%   input
%          sol        temperature solution matrix (ordered by columns)
%          tt         solution time vector  
%          xy         velocity nodal coordinate vector   
%          x          vector of x-axis interpolation points
%          y          vector of y-axis interpolation points
%          ftime      controls speed of animation
%          fig        figure number
%
%   IFISS function: DJS; 3 May 2012.
% Copyright (c) 2009 D.J. Silvester, H.C. Elman, A. Ramage 
fprintf('running temperature field animation ... ')
% interpolate to a cartesian product mesh
[X,Y]=meshgrid(x,y);
nstep=length(tt); umax=max(sol(:,end)); umin=min(sol(:,end));
for k=1:nstep
xysol = griddata(xy(:,1),xy(:,2),sol(:,k),X,Y);
figure(fig), 
subplot(122), contour(X,Y,xysol,30), axis('square'), axis('off')
title(['Isotherms : ',num2str(tt(k),'%8.3f'),' seconds'],'FontSize',12),
subplot(121), mesh(X,Y,xysol),axis('square'),
axis([-1,1,-1,1,umin,umax]),view(220,30),
title(['Temperature : ',num2str(tt(k),'%8.3f'),' seconds'],'FontSize',12), 
drawnow, pause(ftime)
end
fprintf('done\n')
return


