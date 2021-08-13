function solplot_poissoncontrol_ell(sol_y,sol_u,xy,x,y,ell_type,fig)
%SOLPLOT_POISSONCONTROL_ELL plots state and control on L-shaped domain
%   solplot_poissoncontrol_ell(sol,xy,x,y,fig);
%   input
%          sol        nodal solution vector
%          xy         nodal coordinate vector  
%          x          vector of x-axis interpolation points
%          y          vector of y-axis interpolation points
%          ell_type   specifies whether 'L' is on [0,1] or [-1,1]
%          fig        figure number
%
%   IFISS function: JWP, DJS; 30 July 2012.
% Copyright (c) 2012 J.W. Pearson, D.J. Silvester, H.C. Elman, A. Ramage
fprintf('plotting solution... ')
% interpolate to a cartesian product mesh
[X,Y]=meshgrid(x,y);
xysol_y = griddata(xy(:,1),xy(:,2),sol_y,X,Y);
if ell_type==1
    [II,JJ] = find(X<0.5 & Y<0.5); xysol_y(II,JJ) = nan;
elseif ell_type==2
    [II,JJ] = find(X<0 & Y<0); xysol_y(II,JJ) = nan;
end
figure(fig)
subplot(221),contour(X,Y,xysol_y,20),axis('square'),
xlabel('x_1'),ylabel('x_2')
% axis('off')
% if all([min(x),max(x),min(y),max(y)] == [-1,1,-1,1]), squarex, end
subplot(222),mesh(X,Y,xysol_y),axis('square'),
xlabel('x_1'),ylabel('x_2'),zlabel('State y')

xysol_u = griddata(xy(:,1),xy(:,2),sol_u,X,Y);
if ell_type==1
    [II,JJ] = find(X<0.5 & Y<0.5); xysol_u(II,JJ) = nan;
elseif ell_type==2
    [II,JJ] = find(X<0 & Y<0); xysol_u(II,JJ) = nan;
end
subplot(223),contour(X,Y,xysol_u,20),axis('square'),
xlabel('x_1'),ylabel('x_2')
% axis('off')
% if all([min(x),max(x),min(y),max(y)] == [-1,1,-1,1]), squarex, end
subplot(224),mesh(X,Y,xysol_u),axis('square'),
xlabel('x_1'),ylabel('x_2'),zlabel('Control u')

view(330,30)
fprintf('done\n')
return