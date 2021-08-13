function [x,y,xyv,xyp,xyt]=q2q1q2grid(x,y,xy2,xy1,mv2,mp1,mt2,bnd_d,...
                                      bnd_dn2,bnd_dv2,bnd_dnt2,H,L);
%Q2Q1Q2GRID Q2-Q1-Q2 element grid generator 
% [x,y,xyv,xyp,xyt]=q2q1q2grid(x,y,xy2,xy1,mv2,mp1,mt2,bnd_d,bnd_dn2,...
%                               bnd_dv2,bnd_dnt2,H,L);
%   input
%          x          x coordinate vector
%          y          y coordinate vector 
%          xy1        nodal coordinate vector  
%          mv2        Q2 macroelement mapping matrix
%   output
%          xyv        velocity coordinate vector
%          xyp        vertex coordinate vector
%          xyt        temperature coordinate vector
%
%   IFISS function: DJS; 1 May 2012.
% Copyright (c) 2012 D.J. Silvester, M.L. Mihajlovic.


xx=xy2(:,1); yy=xy2(:,2); nvtx=length(xx);
%
%% recompute mid-side points in the case of stretched grids
%
% y-direction 
yv=yy; ny=length(y);
for k=2:2:ny
   yold=y(k); ynew=0.5*(y(k+1)+y(k-1));
   l=find(yy==yold); yv(l)=ynew; y(k)=ynew;
end
% x-direction
xv=xx; nx=length(x);
for k=2:2:nx;
   xold=x(k); xnew=0.5*(x(k+1)+x(k-1));
   l=find(xx==xold); xv(l)=xnew; x(k)=xnew;
end
xyv=[xv,yv];
%
% Plotting the velocity grid
%
adj=sparse(nvtx,nvtx);
mel=length(mv2(:,1)); 
for i=1:mel
   adj(mv2(i,1),mv2(i,2))=1; 
   adj(mv2(i,2),mv2(i,3))=1;
   adj(mv2(i,3),mv2(i,4))=1;
   adj(mv2(i,4),mv2(i,1))=1;
end
figure(1002)
if(H/L>=1)   %  vertical cavity
   set(gcf,'Position',[315,5,825,400]);
else         %  horizontal cavity 
   set(gcf,'Position',[315,5,400,825]); 
end
if(H/L>=1)   %  vertical cavity       
   subplot(131);
else         %  horizontal cavity 
   subplot(311);
end    
gplot(adj,xyv,'b');
axis('square');
hold on;
plot(xyv(:,1),xyv(:,2),'r.');
xyvbd=xyv(bnd_d,:);
plot (xyvbd(:,1),xyvbd(:,2),'k.');
hold off;
title('Q2 velocity dofs');
axis('equal'); axis('off');
drawnow;
%
% Plotting the pressure grid
%
xyp=xy1;
xyt=xyv;
if(H/L>=1)   %  vertical cavity   
   subplot(132);
else         %  horizontal cavity 
   subplot(312);
end   
gplot(adj,xyv,'b');
axis('square');
hold on;
plot(xyp(:,1),xyp(:,2),'m.');
hold off;
title('Q1 pressure dofs');
axis('equal'); axis('off');
drawnow;
%
% Plotting the temperature grid
%
if(H/L>=1)   %  vertical cavity 
   subplot(133);
else         %  horizontal cavity 
   subplot(313);
end     
gplot(adj,xyt,'b');
axis('square');
hold on;
plot(xyt(:,1),xyt(:,2),'c.');
xybdt=xyv(bnd_dn2,:);
plot(xybdt(:,1),xybdt(:,2),'y.');
hold off;
title('Q2 temperature dofs');
axis('equal'); axis('off');
drawnow, pause(10), 
figure(1002), set(gcf,'Visible','off'), drawnow
return         
