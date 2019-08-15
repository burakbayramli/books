function Y = bsp02_f(T,X);
% Example Dyer-McReynolds, p. 128
% State equation

global n t0  t1 U
M = T*n/(t1-t0); J = floor(M) + 1; K = min(n,ceil(M))  + 1;
V = (U(J) + U(K))/2;
Y =  V;
