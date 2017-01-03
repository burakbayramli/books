% normalize the coordinates to zero mean and isotropic scaling 
% and return the normalizing transformation
function [xn, A] = isoscale(xim);
t = mean(xim')';
s = std(xim')';

xn(1,:) = (1/s(1))*(xim(1,:) - t(1));
xn(2,:) = (1/s(2))*(xim(2,:) - t(2));
xn(3,:) = xim(3,:);
% xn
% mm =	mean(xn')'
% ss =	std(xn')'

A = [1/s(1) 0    -t(1)/s(1);
      0   1/s(2) -t(2)/s(2);
      0     0         1];

% A = chol(inv(xim*xim'/size(xim,2)));
% xn = A*xim;

