function [yi,kk] = interpspl(x,y,xi,k0,kend,per)
% cubic interpolatory spline
% computes       yi = spline(xi)
% and slopes     kk = spline'(x)
% where spline(x) = y
% if k0 given, slope at x(1) becomes k0
% if not, not-a-knot condition at x(2)
% similarly at kend, x(end)
% if per = 1 y1 must = yend; if so, periodic
[~,ncols]=size(y);
if nargin < 3
  xi = [];
end
if nargin < 6
  per = 0;
end
if nargin < 5
  kend = NaN(1,ncols);
end
if nargin < 4
  k0 = NaN(1,ncols);
end
m = length(xi);
if per == 1
  if norm(y(end,:)-y(1,:))/norm(y(1,:)) > 1e-14
    error('per 1 but yend ~=y1')
  end
  % cheat by overlapping ...
  x = [2*x(1)-x(2);x;2*x(end)-x(end-1)];
  y = [y(end-1,:);y;y(2,:)];
end
n = length(x)-1;
h = diff(x);
d = diff(y)./(h*ones(1,ncols));
rhs = [3*h(1)*d(1,:);...
    3*(diag(h(1:end-1))*d(2:end,:)+diag(h(2:end))*d(1:end-1,:));
    3*h(n)*d(n,:)];
a = [h(2:end);h(n)];
b = [2*h(1);2*(h(1:n-1)+h(2:n));2*h(n)];
c = [h(1);h(1:n-1)];
if ~isnan(k0(1))
  b(1) = 1;
  c(1) = 0;
  rhs(1,:) = k0;
end;
if ~isnan(kend(1))
  b(n+1) = 1;
  a(n)   = 0;
  rhs(n+1,:) = kend;
end
% Gauss elim tridiag system
for k = 2:n+1
  r      = a(k-1)/b(k-1);
  b(k)   = b(k)-r*c(k-1);
  rhs(k,:) = rhs(k,:)-r*rhs(k-1,:);
end
kk = zeros(n+1,ncols);
kk(n+1,:) = rhs(n+1,:)/b(n+1);
for k = n:-1:1
  kk(k,:) = (rhs(k,:)-c(k)*kk(k+1,:))/b(k);
end
if per == 1
  % remove temp. extensions
  kk = kk(2:end-1,:);
  x  = x(2:end-1);
  y  = y(2:end-1,:);
  n  = length(x)-1;
  h  = h(2:end-1);
  d  = d(2:end-1,:);
end
yi = [];
if ~isempty(xi)
  [xs,ii]=sort(xi);
  i  = 2;
  is = 1;
  yi = zeros(m,ncols);
  while i <= n+1
    while xs(is) <= x(i)
      t = (xs(is)-x(i-1))/h(i-1);
      %disp([is i x(i-1) xs(is) x(i) t])
      yi(is,:) = t*y(i,:)+(1-t)*y(i-1,:)+h(i-1)*t*(1-t)*((kk(i-1,:)-d(i-1,:))*(1-t)-(kk(i,:)-d(i-1,:))*t);
      is = is + 1;
      if is > m
        break
      end
    end
    i = i+1;
  end
  yi = yi(ii,:);
end
