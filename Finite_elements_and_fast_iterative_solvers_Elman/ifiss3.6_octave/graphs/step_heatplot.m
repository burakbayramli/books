function step_heatplot(sol,tt,xy,x,y,ftime,fig)
%STEP_HEATPLOT evolves temperature data on step domain
%   step_heatplot(sol,tt,xy,x,y,ftime,fig)
%   input
%          sol        temperature solution matrix (ordered by columns)
%          tt         solution time vector  
%          xy         velocity nodal coordinate vector   
%          x          vector of x-axis interpolation points
%          y          vector of y-axis interpolation points
%          ftime      controls speed of animation
%          fig        figure number
%
%   IFISS function: DJS; 12 November 2011.
% Copyright (c) 2009 D.J. Silvester, H.C. Elman, A. Ramage 
fprintf('   Running temperature field animation ... ')
outflow=max(x);
% interpolate to a cartesian product mesh
[X,Y]=meshgrid(x,y);
nstep=length(tt); umax=max(sol(:,end)); umin=-0.5; %min(sol(:,1)),
for k=1:nstep
xysol = griddata(xy(:,1),xy(:,2),sol(:,k),X,Y);
[II,JJ]=find(X<0 & Y<0); xysol(II,JJ)=nan;
figure(fig), 
subplot(211), contour(X,Y,xysol,25), axis('equal')
axis([-1,outflow,-1,1]), stepx, axis('off')
title(['Isotherms : ',num2str(tt(k),'%8.3f'),' seconds']),
subplot(212), mesh(X,Y,xysol),axis('equal'),
axis([-1,outflow,-1,1,umin,umax]),view(164,12),  
title(['Temperature : ',num2str(tt(k)),' seconds']),  pause(ftime)
end
fprintf('done\n')
return


