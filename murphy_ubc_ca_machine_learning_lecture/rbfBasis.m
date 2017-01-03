function xout = rbfBasis(xin, centers)
% xin(i,:) for example i
% xout(i,j) = exp(-0.5 * ||xin(i,:) - centers(j,:)||^2 ) if j>1
% xout(i,1) = 1

[N din] = size(xin);
xout = sqdist(xin', centers');
xout = [ones(N,1) exp(-0.5*xout)];
%xout = [exp(-0.5*xout)];
