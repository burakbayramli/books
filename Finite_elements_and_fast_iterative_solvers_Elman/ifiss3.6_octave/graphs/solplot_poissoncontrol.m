function solplot_poissoncontrol(sol_y,sol_u,xy,x,y,fig)
%SOLPLOT_POISSONCONTROL plots state and control on square-shaped domain
%   solplot_poissoncontrol(sol,xy,x,y,fig);
%   input
%          sol        nodal solution vector
%          xy         nodal coordinate vector  
%          x          vector of x-axis interpolation points
%          y          vector of y-axis interpolation points
%          fig        figure number
%
%   IFISS function: JWP, DJS; 30 July 2012.
% Copyright (c) 2012 J.W. Pearson, D.J. Silvester, H.C. Elman, A. Ramage
fprintf('plotting solution... ')
% interpolate to a cartesian product mesh
[X,Y]=meshgrid(x,y);
xysol_y = griddata(xy(:,1),xy(:,2),sol_y,X,Y);
figure(fig)
subplot(221),contour(X,Y,xysol_y,20),axis('square'),
xlabel('x_1'),ylabel('x_2')
% axis('off')
% if all([min(x),max(x),min(y),max(y)] == [-1,1,-1,1]), squarex, end
subplot(222),mesh(X,Y,xysol_y),axis('square'),
xlabel('x_1'),ylabel('x_2'),zlabel('State y')

xysol_u = griddata(xy(:,1),xy(:,2),sol_u,X,Y);
subplot(223),contour(X,Y,xysol_u,20),axis('square'),
xlabel('x_1'),ylabel('x_2')
% axis('off')
% if all([min(x),max(x),min(y),max(y)] == [-1,1,-1,1]), squarex, end
subplot(224),mesh(X,Y,xysol_u),axis('square'),
xlabel('x_1'),ylabel('x_2'),zlabel('Control u')

view(330,30)
fprintf('done\n')
return
