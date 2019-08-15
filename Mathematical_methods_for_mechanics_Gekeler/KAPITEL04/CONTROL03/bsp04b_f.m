function DXDT = bsp04b_f(T,X);
% Bryson-Ho, par. 2.4,
% State equations, same as bsp04a.m

global kappa n t1 U

M  = T*n/t1; J = floor(M) + 1; K = min(n,ceil(M))  + 1;
V  = (U(J) + U(K))/2;
DXDT    =   zeros(4,1);
DXDT(1) =   X(3);
DXDT(2) =   X(4);
DXDT(3) =   kappa*cos(V);
DXDT(4) =   kappa*sin(V);
