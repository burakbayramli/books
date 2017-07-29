function [dflx,dfly] = divflux(P,u,v,dx,dy)
% Approximate flux divergence for field P.
% [dflx,dfly]  returns the numerical divergence of the
% flux of tracer P. dflx corresponds to d(uP)/dx, the differences in the
% x (column) direction. dfly corresponds to d(vP)/dy, the differences
% in the y (row) direction. The spacing between points in the x and y.
% Directions is assumed to be dx and dy respectively.  
[n,p] = size(P);
%compute y component of flux
% Take forward differences on top and bottom edges
dfly(1,:) = 0*(P(2,:).*v(2,:) - P(1,:).*v(1,:))/dy;
dfly(n,:) = 0*(P(n,:).*v(n,:) - P(n-1,:).*v(n-1,:))/dy;

% Take centered differences on interior points

dfly(2:n-1,:) = (P(3:n,:).*v(3:n,:)-P(1:n-2,:).*v(1:n-2,:))/(2*dy);
% compute x component of flux 

% Take cyclic differences on left and right boundaries

dflx(:,1)=(P(:,2).*u(:,2)-P(:,p-1).*u(:,p-1))/(2*dx);
dflx(:,p)= dflx(:,1);

% take centered differences on interior points

dflx(:,2:p-1)= (P(:,3:p).*u(:,3:p)-P(:,1:p-2).*u(:,1:p-2))/(2*dx);
