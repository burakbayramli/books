function Z = bsp01_h(flag);
% Dyer-McReynolds, p. 127, Example 1
% flag = 1: Objective function with trapezoidal rule
% flag = 2; DH/DU

global n t0 t1 U X Y
switch flag
case 1
   A = (t1 - t0)/(2*n);
   INTEGRAND = X(1,:).*X(1,:) + U.*U;
   FAKTOR    = 2*ones(n+1,1);
   FAKTOR(1) = 1; FAKTOR(n+1) = 1;
   Z = A*INTEGRAND*FAKTOR;
case 2   
   Z = 2*U + Y(1,:) - Y(2,:);
end
