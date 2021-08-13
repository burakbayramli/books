function [w]=step_vorticityplot(qmethod,sol,By,Bx,G,xy,x,y,bound,spc,fig)
%STEP_VORTICITYPLOT plots vorticity data on step domain
%   [w]= step_vorticityplot(qmethod,xns,By,Bx,G,xy,x,y,bound,1,69);
%   input
%          qmethod    mixed method 
%          sol        flow solution vector
%          By         velocity  y-derivative matrix    
%          Bx         velocity x-derivative matrix    
%          G          vector velocity mass matrix
%          xy         velocity nodal coordinate vector  
%          x          vector of x-axis interpolation points
%          y          vector of y-axis interpolation points
%          bound      boundary vertex vector
%          spc        uniform/nonuniform contour switch 
%          fig        figure number
%   output
%          w          vorticity vector   
%
%   IFISS function: DJS; 30 April 2012.
% Copyright (c) 2012 D.J. Silvester, H.C. Elman, A. Ramage 
nvtx=length(xy); nu=2*nvtx; 
% compute auxilliary quantites
u=sol(1:nu);
f=[-By,Bx]*u;
w=G(1:nvtx,1:nvtx)\f;
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
[II,JJ]=find(X<0 & Y<0); xysol(II,JJ)=nan;
if spc==1,
   subplot(211),contour(X,Y,xysol,20),	
   title('Vortexlines: uniform','FontSize',12); 
   if all([min(x),max(x),min(y),max(y)] == [-1,5,-1,1]), stepx, end
   axis('off')
elseif spc==2,
   v=[-15:1.3:6]';v=exp(v); list=sort([-v;v]);
   subplot(211),contour(X,Y,xysol,list),
   title('Vortexlines: selected','FontSize',12); 
   if all([min(x),max(x),min(y),max(y)] == [-1,5,-1,1]), stepx, end
end	
subplot(212), mesh(X,Y,xysol), 
 title('Vorticity distribution','FontSize',12); 
if all([min(x),max(x),min(y),max(y)] == [-1,5,-1,1]), stepx, end
axis('off')
return
