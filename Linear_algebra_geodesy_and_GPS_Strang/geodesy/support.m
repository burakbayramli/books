function support(A)
%SUPPORT Plot of support function and pertinent confidence
%     	ellipse for a given 2 by 2 covariance matrix A

%Kai Borre 
%Copyright (c) by Kai Borre
%$Revision: 1.0 $  $Date: 1997/09/26  $

delete support.eps
[m,n] = size(A);
if m ~= 2 | n ~= 2
   error('Wrong dimension of matrix');
end
[v,d] = eig(A);
if d(1,1) <= 0 | d(2,2) <= 0
   error('The input matrix is no covariance matrix'); 
end;

% Calculations for confidence ellipse
[lambda,k] = sort(diag(d));
v = v(:,k);
if any(any(v)) == 1
   alpha = atan2(v(2,2),v(1,2));
else 
   alpha = 0;
end
rot = [cos(alpha) sin(alpha);-sin(alpha) cos(alpha)];
t = linspace(0,2*pi,100);
a = sqrt(lambda(2));
b = sqrt(lambda(1));
pl = [a*cos(t);b*sin(t)];
for t = 1:100
   current = rot*pl(:,t); curve(1:2,t) = current; 
end

% Calculations for support function
phi = linspace(0,2*pi,100);
support = sqrt(A(1,1)*(cos(phi)).^2 + A(2,1)*sin(2*phi)...
   + A(2,2)*(sin(phi)).^2);

% The 1-axis is oriented upwards and the 2-axis towards the right.
% In the polar plot we add pi/2 and in the cartesian plot
% interchanged the 1 and 2 columns of curve
h = figure(1);
hold on
axis([-1.5*a 1.5*a -1.5*a 1.5*a])
axis('equal')
polar(phi+pi/2,support,'-')
axis(axis)
plot(curve(2,1:100),curve(1,1:100),'--')
hold off
print support -deps
%%%%%%%% end support.m %%%%%%%%%%%%%%%%%%%
