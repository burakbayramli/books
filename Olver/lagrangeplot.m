function lagrangeplot(xd,f,x0,x1,n)

%
%  lagrangeplot(xd,f,x0,x1,n)
%  
%     Plot Lagrange interpolating polynomial for function f
%
%     xd    -- sample points (row vector)
%     f     -- function (string)
%     x0,x1 -- plot range -- default is first and last entries of x
%     n     -- number of plot points --- default is 100.
%

k = size(xd,2);

if nargin <= 4 n=100; end
if nargin <= 2 x0 = xd(1); x1 = xd(k); end

yd = feval(f,xd);
x = linspace(x0,x1,n);
y = lagrangep(x,xd,yd);
z = feval(f,x);
hold off;
plot(x,y);
axis tight;
hold on;
axis manual;
plot(xd,yd,'ro');
plot(x,z,'g');
