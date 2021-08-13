function [w]=square_vorticityplot(qmethod,sol,By,Bx,G,xy,x,y,bound,spc,fig)
%SQUARE_VORTICITYPLOT plots vorticity data on square domain
%   [w]= square_vorticityplot(qmethod,xns,By,Bx,G(1:nv,1:nv),xy,x,y,bound,1,69);
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
if spc==1,
   subplot(121),contour(X,Y,xysol,20),axis('square')	
   title('Vortexlines: uniform'); 
   if all([min(x),max(x),min(y),max(y)] == [-1,1,-1,1]), squarex, end
elseif spc==2,
   v=[-15:1.3:6]';v=exp(v); list=sort([-v;v]);
   subplot(121),contour(X,Y,xysol,list),axis('square')
   title('Vortexlines: selected'); 
   if all([min(x),max(x),min(y),max(y)] == [-1,1,-1,1]), squarex, end
end	
subplot(122), mesh(X,Y,xysol), axis('square')
 title('Vorticity distribution'); 
axis('off')
return
