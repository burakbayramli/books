function step_unsteadytempref(sol,tt,xy,x,y,snaptime)
%STEP_UNSTEADYTEMPREF plots temperature data at snapshot times
%   step_unsteadytempref(sol,tt,xy,x,y,snaptime)
%   input
%          sol        temperature solution matrix (ordered by columns)
%          tt         solution time vector  
%          xy         velocity nodal coordinate vector   
%          x          vector of x-axis interpolation points
%          y          vector of y-axis interpolation points
%          snaptime   vector of snapshot time step levels
%
%   IFISS function: DJS; 27 July 2015
% Copyright (c) 2011 D.J. Silvester, H.C. Elman, A. Ramage 
fprintf('   Plotting temperature snapshots ... ')
L=max(x); nstep=length(snaptime);
if nstep>9, error('Too many snapshots!'), end
outflow=max(x);
% interpolate to a cartesian product mesh
[X,Y]=meshgrid(x,y);
umax=max(sol(:,end)); umin=min(sol(:,1));
for k=1:nstep
indx=100*nstep +10 +k;
subplot(indx)
kk=snaptime(k); ttk=tt(kk);
xysol = griddata(xy(:,1),xy(:,2),sol(:,kk),X,Y);
[II,JJ]=find(X<0 & Y<0); xysol(II,JJ)=nan;
figure(105),
colormap jet
contour(X,Y,xysol,30), axis('equal')
axis([-1,outflow,-1,1]), stepx, axis('off')
title(['Isotherms: time = ',num2str(ttk,'%5.2f')],'FontSize',10),  
end
% ------------------ end loop over snapshots
%
fprintf('   All done\n')
return
