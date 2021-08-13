function [w]=obstacle_vorticityplot(qmethod,sol,By,Bx,G,xy,x,y,...
				bound,bndxy,bnde,obs,spc,fig)
%OBSTACLE_VORTICITYPLOT plots vorticity data in obstructed channel
%   [w]= obstacle_vorticityplot(qmethod,xns,By,Bx,G(1:nv,1:nv),xy,x,y,...
%								bound,bndxy,bnde,obs,1,69);
%   input
%          qmethod    mixed method 
%          sol        flow solution vector
%          By         velocity  y-derivative matrix    
%          Bx         velocity x-derivative matrix    
%          G          scalar velocity mass matrix
%          xy         velocity nodal coordinate vector  
%          x          vector of x-axis interpolation points
%          y          vector of y-axis interpolation points
%          bound      boundary vertex vector
%          spc        uniform/nonuniform contour switch 
%          fig        figure number
%   output
%          w          vorticity vector   
%
%   IFISS function: DJS; 18 January 2010.
% Copyright (c) 2009 D.J. Silvester, H.C. Elman, A. Ramage 
nvtx=length(xy); nu=2*nvtx; 
%Asv=A(1:nvtx,1:nvtx);
%
% compute auxilliary quantites
u=sol(1:nu);
f=[-By,Bx]*u;
%[Asv,fsv] = nonzerobc(Asv,f,xy,bound); phi=Asv\fsv;
w=G\f;
%
%% plot vorticity
if qmethod==2
   xx=x(1:2:end); yy=y(1:2:end);
elseif qmethod==3
   xx=x(1:end); yy=y(1:end);
else
   xx=x(1:end); yy=y(1:end); 
end
figure(fig)
[X,Y]=meshgrid(x,y);
xysol = griddata(xy(:,1),xy(:,2),w,X,Y);
if size(obs,1)~=0
[II,JJ] = findobsXY(obs,X,Y,bndxy); xysol(II,JJ)=nan;
end
if spc==1,
   subplot(211),contour(X,Y,xysol,20),	
   title('Vortexlines: uniform'); 
   axis('off')
elseif spc==2,
   v=[-15:1.3:6]';v=exp(v); list=sort([-v;v]);
   subplot(211),contour(X,Y,xysol,list),
   title('Vortexlines: selected'); 
end	
hold on
for i = 1:size(bnde,1)
plot([bndxy(bnde(i,1),1), bndxy(bnde(i,2),1)],[bndxy(bnde(i,1),2),bndxy(bnde(i,2),2)],'-k')
end
hold off
axis([-0.5,8.5,-1.5,1.5]) 
subplot(212), mesh(X,Y,xysol), 
 title('Vorticity distribution'); 
hold on
for i = 1:size(bnde,1)
plot([bndxy(bnde(i,1),1), bndxy(bnde(i,2),1)],[bndxy(bnde(i,1),2),bndxy(bnde(i,2),2)],'-k')
end
hold off
axis('off')
return
