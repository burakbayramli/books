function [delx,dely] = grad(P,dx,dy)
% GRADIENT Approximate gradient.
% [FX,FY] = GRAD(F,dx,dy) returns the numerical gradient of the
% matrix F. FX corresponds to dF/dx, the differences in the
% x (column) direction. FY corresponds to dF/dy, the differences
% in the y (row) direction. The spacing between points in the x and y
% directions is assumed to be dx and dy respectively. This version
% is a modification of the standard function for cyclic boundary conditions
% in the zonal direction.

[n,p] = size(P);
% Compute y component of gradient
% Take forward differences on top and bottom edges
dely(1,:) = (P(2,:) - P(1,:))/dy;
dely(n,:) = (P(n,:) - P(n-1,:))/dy;

% Take centered differences on interior points

dely(2:n-1,:) = (P(3:n,:)-P(1:n-2,:))/(2*dy);
%compute x component of gradient 

% Take cyclic differences on left and right boundaries

delx(:,1)=(P(:,2)-P(:,p-1))/(2*dx);
delx(:,p)= delx(:,1);

% Take centered differences on interior points

delx(:,2:p-1)= (P(:,3:p)-P(:,1:p-2))/(2*dx);
