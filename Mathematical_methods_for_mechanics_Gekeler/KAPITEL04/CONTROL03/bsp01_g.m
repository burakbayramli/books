function DYDT = bsp01_g(T,Y);
% Dyer-McReynolds, p. 127, Example 1
% Costate equation
global n t0 t1 X U
M = T*n/(t1-t0); J = floor(M) + 1; K = min(n,ceil(M))  + 1;
V = (X(:,J) + X(:,K))/2;
DYDT = zeros(2,1); DYDT(1) = - 2*V(1); DYDT(2) = - Y(1);
