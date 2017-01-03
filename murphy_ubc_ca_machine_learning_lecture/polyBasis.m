function X = polyBasis(x, d)
x = x(:);
n = size(x,1);
X = ones(n,1);
%X = [];
for j=1:d,
  X=[X x.^j];
end;
%X = [X ones(n,1)];
