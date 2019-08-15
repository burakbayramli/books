function Z = bsp02_h(flag);
% Example Dyer-McReynolds, p. 128
% flag = 1: Objective function with trapezoidal rule
% flag = 2; DH/DU

global n t0 t1 U X Y
if flag == 1
   A           = (t1 - t0)/(2*n);
   FAKTOR      = 2*ones(n+1,1);
   FAKTOR(1)   = 1;
   FAKTOR(n+1) = 1;
   INTEGRAND   = sqrt(1 + U.*U)./sqrt(1 - X);
   Z = A*INTEGRAND*FAKTOR;
end
if flag == 2
   Z = Y + U./(sqrt(1 + U.*U).*sqrt(1 - X));
end
if flag == 3
   Z = X(n+1) + 0.5;
end
