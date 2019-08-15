function DXDT = bsp01_f(T,X);
% Dyer-McReynolds, p. 127, Example 1,
% State equation

global n t0  t1 U
M = T*n/(t1-t0); J = floor(M) + 1; K = min(n,ceil(M))  + 1;
V = (U(J) + U(K))/2;
DXDT = zeros(2,1); DXDT(1) = X(2) + V; DXDT(2) = - V;
