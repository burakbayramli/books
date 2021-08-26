function [D] = FourierD(N,q);
% function [D] = FourierD(N);
% Purpose: Initialize q'th order Fourier differentiation matrix
column = [0 (-1).^(1:2*N)./(2*sin(pi/(2*N+1)*(1:2*N)))]';
D = toeplitz(column, column([1 (2*N+1):-1:2]))^q;
return