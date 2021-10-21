clf
x = linspace(0,4,500);
u = 1-.5*sin(2*pi*x);
plot(x,u,'LineWidth',2)
axis([0 4 0 2])
title('velocity u(x)')

print u.eps

