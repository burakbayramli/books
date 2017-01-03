function [c,ceq]=f722c(x)
c=[-x(1); -x(2); 3*x(1)-x(1)*x(2)+4*x(2)-7;
    2*x(1)+x(2)-3; 3*x(1)-4*x(2)^2-4*x(2)]; %inequality constraints
ceq=[]; %equality constraints 
