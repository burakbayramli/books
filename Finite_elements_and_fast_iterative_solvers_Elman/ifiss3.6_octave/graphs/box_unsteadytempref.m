function box_unsteadytempref(T,time,xyt,x,y,L,H,hty,snaptime,fig)
%BOX_UNSTEADYTEMPREF plots cavity flow data at snapshot times 
%   box_unsteadytempref(T,soltime,xyt,x,y,L,H,hty,snaptime,fig)
%   input
%          T          temperature solution matrix (ordered by columns)
%          soltime    solution time vector  
%          xyt        temperature nodal coordinate vector   
%          x          vector of x-axis interpolation points
%          y          vector of y-axis interpolation points
%          L,H        domain coordinates
%          hty        horizontal/vertical (1/2) hot wall switch 
%          snaptime   vector of snapshot time step levels
%          fig        figure number
%
% calls function boxx.m to color code imposed temperature
%   IFISS function: DJS; 30 September 2013.
% Copyright (c) 2011 D.J. Silvester, H.C. Elman, A. Ramage 
fprintf('\n   Plotting temperature field snapshots ... ')
L=max(x); H=max(y); nstep=length(snaptime);
if nstep>9, error('Too many snapshots!'), end
nvtx=length(xyt); 
[X,Y]=meshgrid(x,y);
figure(fig) 
ax = [min(x)-.1 max(x)+.1 min(y)-.1 max(y)+.1];
fprintf('\n   step   time     min_T     max_T\n')
%
% ------------------ loop over snapshots
for k=1:nstep
kk=snaptime(k); ttk=time(kk);
tt=T(:,kk); 
xysol = griddata(xyt(:,1),xyt(:,2),tt,X,Y);
%if L<=H,subplot(121), else subplot(211), end
indx=100*nstep +10 +k;
subplot(indx)
contour(X,Y,xysol,15), %axis('tight'),  
axis equal, axx=ax(1:4); axis(axx(1:4)); axis('off')
boxx
title(['Isotherms: time = ',num2str(ttk,'%5.2f')],'FontSize',12),  
fprintf('  %4i  %7.3f %8.3e  %8.3e\n', kk, ttk,  min(tt), max(tt));
end
% ------------------ end loop over snapshots
%
fprintf('   All done\n')
return
