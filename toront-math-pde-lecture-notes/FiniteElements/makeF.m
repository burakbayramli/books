% I = alt_makeF(@f,x0,x1) uses Gauss quadrature to approximate the
% integral of f from x0 to x1
%
function F = alt_makeF(f,x,dx);

X(1) = 0;
W(1) = 128/225;
X(2) = sqrt(5-2*sqrt(10/7))/3;
W(2) = (322+13*sqrt(70))/900;
X(3) = - X(2);
W(3) = W(2);
X(4) = sqrt(5+2*sqrt(10/7))/3;
W(4) = (322-13*sqrt(70))/900;
X(5) = - X(4);
W(5) = W(4);

n = length(x)-1;
for j=1:n-1
    % lefthand bound of integration
    x0 = x(j);
    % righthand bound of integration
    x1 = x(j+1);
    xx = (x0+x1)/2 + X*(x1-x0)/2;
    % the integrand is f times the basis function phi_j
    y = f(xx);
    y = y.*(xx-x0)/(x1-x0);
    I = dot(W,y);
    F(j) = I*(x1-x0)/2;
    x0 = x(j+1);
    x1 = x(j+2);
    xx = (x0+x1)/2 + X*(x1-x0)/2;
    % the integrand is f times the basis function phi_j
    y = f(xx);
    y = y.*(xx-x1)/(x0-x1);
    I = dot(W,y);
    I = I*(x1-x0)/2;
    F(j) = F(j)+I;
end
j = n;
x0 = x(j);
x1 = x(j+1);
xx = (x0+x1)/2 + X*(x1-x0)/2;
% the integrand is f times the basis function phi_j
y = f(xx);
y = y.*((xx-x0)/(x(j+1)-x0));
I = dot(W,y);
F(j) = I*(x1-x0)/2;



    