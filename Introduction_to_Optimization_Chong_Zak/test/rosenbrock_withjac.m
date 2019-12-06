function [y,gy] =rosenbrock_withjac(x);

y = 100*(x(2)-x(1).^2).^2+(1-x(1)).^2;

gy =[-400*(x(2)-x(1)^2)*x(1)-2*(1-x(1)) ; 200*(x(2)-x(1)^2)];
