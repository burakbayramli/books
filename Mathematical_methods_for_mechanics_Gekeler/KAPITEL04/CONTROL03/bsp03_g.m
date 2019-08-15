function DYDT = bsp03_g(T,Y);
% Dyer-McReynolds, p. 73
% Costate equations

global n t1 X

BB = 1;
M  = T*n/t1; J  = floor(M) + 1; K = min(n,ceil(M))  + 1;
V  = (X(:,J) + X(:,K))/2;
DYDT = zeros(3,1);
DYDT(1) =   Y(2)*V(3)*V(3)/(V(1)*V(1)) - 2*Y(2)/V(1)^3 ...
          - BB*Y(3)*V(2)*V(3)/(V(1)*V(1));
DYDT(2) =   BB*Y(3)*V(3)/V(1) - Y(1);
DYDT(3) =   BB*Y(3)*V(2)/V(1) - 2*Y(2)*V(3)/V(1);
