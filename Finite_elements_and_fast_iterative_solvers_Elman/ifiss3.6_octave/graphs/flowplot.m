function flowplot(qmethod,sol,By,Bx,A,xy,xyp,x,y,bound,spc,fig)
%FLOWPLOT IFISS2.2 version
%   flowplot(qmethod,sol,By,Bx,A,xy,xyp,x,y,bound,spc,fig);
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
%          spc        uniform/nonuniform streamline switch 
%          fig        figure number
%
% calls function streambc.m to set boundary values
%   IFISS function: DJS; 30 April 2012.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
nvtx=length(xy); nu=2*nvtx; np=length(xyp);
Asv=A(1:nvtx,1:nvtx);
%
% compute auxilliary quantites
u=sol(1:nu);p=sol(nu+1:end);
f=[By,-Bx]*u;
[Asv,fsv] = streambc(Asv,f,xy,bound);
phi=Asv\fsv;
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
figure(fig)
subplot(122), mesh(X,Y,xysol),axis('square')
title('pressure field','FontSize',12)
%
%% plot velocity
ax = [min(x)-.1 max(x)+.1 min(y)-.1 max(y)+.1];
[X,Y]=meshgrid(x,y);
xysol = griddata(xy(:,1),xy(:,2),phi,X,Y);
maxphi=max(max(xysol)); minphi=min(min(xysol));
if spc==1,
   subplot(121),contour(X,Y,xysol,24),axis('square'), axis(ax)	
   title('Streamlines: uniform','FontSize',12); 
   if all([min(x),max(x),min(y),max(y)] == [-1,1,-1,1]), squarex, end
elseif spc==2,
   vneg=[minphi:-minphi/24:0];
   vpos=[0:maxphi/9:maxphi];
   subplot(121)
   contour(X,Y,xysol,[vneg,vpos])
   axis('square'), axis(ax),
   title('Streamlines: selected','FontSize',12); 
   if all([min(x),max(x),min(y),max(y)] == [-1,1,-1,1]), squarex, end
end	
axis('off')
return
