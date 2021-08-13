function box_heatplot(sol,tt,xy,x,y,L,H,ftime,fig)
%BOX_HEATPLOT evolves temperature data on cavity domain
%   box_heatplot(sol,tt,xy,x,y,L,H,ftime,fig)
%   input
%          sol        temperature solution matrix (ordered by columns)
%          tt         solution time vector  
%          xy         velocity nodal coordinate vector   
%          x          vector of x-axis interpolation points
%          y          vector of y-axis interpolation points
%          L          box width
%          H          box height
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
subplot(122), contour(X,Y,xysol,30),axis('image'), axis('off')
title(['Isotherms : ',num2str(tt(k),'%8.3f'),' seconds'],'FontSize',12),
subplot(121), mesh(X,Y,xysol),axis('image'),
axis([0,L,0,H,umin,umax]),view(145,15),
title(['Temperature : ',num2str(tt(k),'%8.3f'),' seconds'],'FontSize',12),  
drawnow, pause(ftime)
end
fprintf('done\n')
return


