function DXDT = bsp03_f(T,XX);
% Dyer-McReynolds, p. 73,
% State equations

global kappa n t1 U
BB = 1;
M  = T*n/t1; J = floor(M) + 1; K = min(n,ceil(M))  + 1;
V  = (U(J) + U(K))/2;
DXDT    =   zeros(3,1);
DXDT(1) =   XX(2);
DXDT(2) =   XX(3)*XX(3)/XX(1) - 1/(XX(1)*XX(1)) + kappa*sin(V);
DXDT(3) = - BB*XX(2)*XX(3)/XX(1) + kappa*cos(V);
