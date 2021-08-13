function flowplotz(qmethod,sol,By,Bx,A,xy,xyp,x,y,bound,fig)
%FLOWPLOTZ plots flow data on extended step domain
%   flowplotz(qmethod,sol,By,Bx,A,xy,xyp,x,y,bound,fig);
%   input
%          qmethod    mixed method 
%          sol        flow solution vector
%          By         velocity  y-derivative matrix    
%          Bx         velocity x-derivative matrix    
%          A          vector diffusion matrix
%          xy         velocity nodal coordinate vector  
%          xyp        pressure nodal coordinate vector  
%          x          vector of x-axis interpolation points
%          y          vector of y-axis interpolation points
%          bound      boundary vertex vector
%          fig        figure number
%
% pressure solution is assumed to be essentially zero at outflow 
% so streamfunction satisfies zero Neumann condition there
% calls function streambc.m to set boundary values
%   IFISS function: DJS; 30 April 2012.
% Copyright (c) 2009 D.J. Silvester, H.C. Elman, A. Ramage 
L=max(x);
nvtx=length(xy); nu=2*nvtx; np=length(xyp);
Asv=A(1:nvtx,1:nvtx);
% compute auxilliary quantites
u=sol(1:nu);p=sol(nu+1:end);
f=[By,-Bx]*u;
[Asv,fsv]=streambc(Asv,f,xy,bound);
phi=Asv\fsv;
fprintf('\nminimum value of stream function is  %8.3e',min(phi))
fprintf('\n                         maximum is  %8.3e\n',max(phi))
%
%% plot pressure
if qmethod==2
   xx=x(1:2:end); yy=y(1:2:end);
   elseif qmethod==3
   p=p(1:3:end); xx=x(1:end); yy=y(1:end);
else
   xx=x(1:end); yy=y(1:end);
end
% interpolate to a cartesian product mesh
[X,Y]=meshgrid(xx,yy);
xysol = griddata(xyp(:,1),xyp(:,2),p,X,Y);
[II,JJ]=find(X<0 & Y<0); xysol(II,JJ)=nan;
solheight = max(max(xysol))-min(min(xysol));
ax5 = min(min(xysol))-.1*solheight;
ax6 = max(max(xysol))+.1*solheight;
ax = [min(xx)-1 max(xx)+1 min(yy)-1 max(yy)+1 ax5 ax6];
figure(fig)
subplot(212), mesh(X,Y,xysol), axis(ax)
title('Pressure field [Navier-Stokes]','FontSize',12)
view(350,10)
%
%% plot velocity
[X,Y]=meshgrid(x,y);
xysol = griddata(xy(:,1),xy(:,2),phi,X,Y);
maxphi=max(max(xysol)); minphi=min(min(xysol));
vneg=[minphi:-minphi/6:0];
vpos=[maxphi/20:maxphi/20:19*maxphi/20];
vpospos=[79*maxphi/80: maxphi/320:maxphi];
subplot(211)
if L<=5 %default domain
   contour(X,Y,xysol,[vneg,vpos])
else
   contour(X,Y,xysol,[vneg,vpos,vpospos])
  %contour(X,Y,xysol,[vneg,vpospos])
   axis equal, axx=ax(1:4); axx(2)=min(25,axx(2));
   axis(axx(1:4)); 
end
title('Streamlines: non-uniform [Navier-Stokes]','FontSize',12)
stepx; axis('off')
return
