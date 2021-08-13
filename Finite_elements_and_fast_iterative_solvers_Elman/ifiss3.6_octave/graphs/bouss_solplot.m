function bouss_solplot(domain,U,T,time,xyv,xyt,x,y,L,H,hty,ftime,fig)
%BOUSS_SOLPLOT plots solution evolution on general domain
%   bouss_solplot(domain,U,T,time,xyv,xyt,x,y,L,H,hty,ftime,fig)
%   input
%          domain     problem domain
%          U          velocity solution matrix (ordered by columns)
%          T          temperature solution matrix (ordered by columns)
%          time       solution time vector  
%          xy         velocity nodal coordinate vector   
%          x          vector of x-axis interpolation points
%          y          vector of y-axis interpolation points
%          L,H        domain coordinates
%          hty        horizontal/vertical (1/2) hot wall switch 
%          ftime      controls speed of animation
%          fig        figure number
%
%   IFISS function: DJS; 4 May 2012.
% Copyright (c) 2012 D.J. Silvester, M.L. Mihajlovic.
fprintf('running  Boussinesq flow animation ... ')
nvtx=length(xyv);  nv=2*nvtx; 
[X,Y]=meshgrid(x,y);
nstep=length(time); 
figure(fig) 
ax = [min(x)-.1 max(x)+.1 min(y)-.1 max(y)+.1];
for k=1:nstep
u=U(:,k); 
ux=u(1:nvtx); uy=u(nvtx+1:nv); uu=sqrt(ux.*ux+uy.*uy);
umax=max(U(:,end)); umin=min(U(:,end));
xysol = griddata(xyv(:,1),xyv(:,2),uu,X,Y);
if domain==3,[II,JJ]=find(X<0 & Y<0); xysol(II,JJ)=nan; end
if L<=H,subplot(122), else subplot(212), end
contour(X,Y,xysol,25), %axis('tight'),  
axis equal, axx=ax(1:4); axx(2)=min(25,axx(2)); axis(axx(1:4)); axis('off')
if domain==3, stepx, else, boxx, end
title(['velocity magnitude : ',num2str(time(k)),' seconds'],'FontSize',12),
tt=T(:,k); 
tmax=max(T(:,end)); tmin=min(T(:,end));
xysol = griddata(xyt(:,1),xyt(:,2),tt,X,Y);
if domain==3,[II,JJ]=find(X<0 & Y<0); xysol(II,JJ)=nan; end
if L<=H,subplot(121), else subplot(211), end
contour(X,Y,xysol,25), %axis('tight'),  
axis equal, axx=ax(1:4); axx(2)=min(25,axx(2)); axis(axx(1:4)); axis('off')
if domain==3, stepx, else, boxx, end
title(['isotherms : ',num2str(time(k)),' seconds'],'FontSize',12),  
drawnow, pause(ftime)
end
fprintf('done\n')
return


