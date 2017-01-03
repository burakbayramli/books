function D = sqdist(x,y)
%SQDIST Square distance between vectors in x and y
% D = sqdist(x,y)
[d m]=size(x); n=size(y,2);
xx=repmat(sum(x.^2,1),n,1); yy=repmat(sum(y.^2,1),m,1); xy=x'*y; D = xx'+yy-2*xy;