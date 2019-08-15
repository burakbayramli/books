function Z = bsp02_g(T,Y);
% Example Dyer-McReynolds, p. 128
% Costate equation 

global n t0 t1 X U
M = T*n/(t1-t0); J = floor(M) + 1; K = min(n,ceil(M))  + 1;
V = (U(:,J) + U(:,K))/2;
W = (X(:,J) + X(:,K))/2;
Z = - 0.5*sqrt(1 + V*V)/(sqrt(1-W))^3;
