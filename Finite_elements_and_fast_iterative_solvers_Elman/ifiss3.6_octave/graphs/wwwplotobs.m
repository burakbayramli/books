function wwwplotobs(qmethod,sol,eldata,ev,By,Bx,A,xy,xyp,x,y,bound,bndxy,bnde,obs,contourn,spc,fig)
%WWWPLOTOBS streamlines and error on general domain
%   wwwplotobs(qmethod,xns,error_tot,mv(:,1:4),By,Bx,A,xy,xyp,x,y,bound,bndxy,bnde,obs,contourn,1,64)
%   input
%          qmethod    mixed method 
%          xns        flow solution vector
%          error_tot  element error vector
%          ev/mv      element mapping matrix
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
%   IFISS function: HCE; DJS; 27 April 2012.
%   Adapted from code written by M. Wu, 2009
% Copyright (c) 2011 D.J. Silvester, H.C. Elman, A. Ramage;
%
if ishandle(fig), clf(fig); end
figure(fig)
nvtx=length(xy); nu=2*nvtx;   %np=length(xyp);
Asv=A(1:nvtx,1:nvtx);
%
%% compute auxiliary quantities
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
%
%% plot velocity
[X,Y]=meshgrid(x,y);
xysol = griddata(xy(:,1),xy(:,2),phi,X,Y);
if size(obs,1)~=0
   [II,JJ] = findobsXY(obs,X,Y,bndxy); xysol(II,JJ)=nan;
end
if spc==1
   subplot(211),contour(X,Y,xysol,contourn),axis('tight'), %colorbar('EastOutside')
   title('Flow Solution Streamlines'); 
elseif spc == 2
   cn = fix(contourn/2-1);
   ch = 21/cn;
   v=(-15:ch:6)';v=exp(v); list=sort([-v;v]);
   subplot(211),contour(X,Y,xysol,list),axis('tight'),  %colorbar('EastOutside')
   title('Flow Solution Streamlines'); 
end
axis('off')
%
% plot the boundary
hold on
for i = 1:size(bnde,1)
   plot([bndxy(bnde(i,1),1), bndxy(bnde(i,2),1)],[bndxy(bnde(i,1),2),bndxy(bnde(i,2),2)],'-k')
end
hold off
%
% plot the estimated error
%eplot(error_tot,mv(:,1:4),xy,x(1:2:end),y(1:2:end),67); 
xx=xy(:,1); yy=xy(:,2);
nel=length(eldata);
xc = zeros(nel,2);
% loop over elements    
for ielem = 1:nel
xl = xx(ev(ielem,:));
yl = yy(ev(ielem,:)); 
xc(ielem,1) = 0.25*sum(xl);
xc(ielem,2) = 0.25*sum(yl);
end
%
% interpolate to a cartesian product mesh
x=0.5*(x(1:end-1)+x(2:end));
y=0.5*(y(1:end-1)+y(2:end));
[X,Y]=meshgrid(x,y);
xysol = griddata(xc(:,1),xc(:,2),eldata,X,Y);
subplot(212),contour(X,Y,xysol,25),axis([0,8,-1,1]), axis('off')
title('Flow Solution Estimated Error')
%
% plot the boundary
hold on
for i = 1:size(bnde,1)
plot([bndxy(bnde(i,1),1), bndxy(bnde(i,2),1)],[bndxy(bnde(i,1),2),bndxy(bnde(i,2),2)],'-k')
end
hold off
return
