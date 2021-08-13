function flowplotp(qmethod,sol,By,Bx,A,xy,xyp,x,y,bound,fig);
%FLOWPLOTP plots flow data on slit shaped domain
%   flowplotp(qmethod,sol,By,Bx,A,xy,xyp,x,y,bound,fig);
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
% calls function streambc.m to set boundary values
%   IFISS function: DJS; 30 April 2012.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
nvtx=length(xy); nu=2*nvtx; np=length(xyp);
Asv=A(1:nvtx,1:nvtx);
% compute auxilliary quantites
u=sol(1:nu);
f=[By,-Bx]*u;
[Asv,fsv] = streambc(Asv,f,xy,bound);
phi=Asv\fsv;
%
%%% plot velocity
[X,Y]=meshgrid(x,y);
xysol = griddata(xy(:,1),xy(:,2),phi,X,Y);
maxphi=max(max(xysol)); minphi=min(min(xysol));
vneg=[minphi:-minphi/12:0];vpos=[maxphi/12:maxphi/12:maxphi];
figure(fig)
subplot(121),contour(X,Y,xysol,[vneg,vpos]), axis('square')
   title('Streamlines: uniform','FontSize',12);
axis('off')
xysol = griddata(xy(:,1),xy(:,2),u(1:nvtx),X,Y);
subplot(122),contour(X,Y,xysol,[0:0.19:0.95]), axis('square')
   title('Horizontal velocity contours','FontSize',12);
return
