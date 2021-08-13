function ell_heatplot(sol,tt,xy,x,y,ftime,fig)
%ELL_HEATPLOT evolves temperature data on L-shaped domain
%   ell_heatplot(sol,tt,xy,x,y,ftime,fig)
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
nstep=length(tt); umax=max(sol(:,end)); umin=0; %min(sol(:,1)),
for k=1:nstep
xysol = griddata(xy(:,1),xy(:,2),sol(:,k),X,Y);
[II,JJ]=find(X<0 & Y<0); xysol(II,JJ)=nan;
figure(fig), 
subplot(122), contour(X,Y,xysol,25), axis('square')
axis([-1.1,1.1,-1.1,1.1]), ellx, axis('off')
title(['Isotherms : ',num2str(tt(k),'%8.3f'),' seconds'],'FontSize',12),
subplot(121), mesh(X,Y,xysol),axis('square'),
axis([-1,1,-1,1,umin,umax]),view(130,30), 
title(['Temperature : ',num2str(tt(k)),' seconds'],'FontSize',12),  
drawnow, pause(ftime)
end
fprintf('done\n')
return


