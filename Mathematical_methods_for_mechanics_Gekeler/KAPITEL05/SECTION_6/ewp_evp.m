function [A,EW,EV1,EV2] = ewp_evp(n)
% Calculates for Laplace operator in unit square:
% 1. Matrix A of finite difference
% 2. discrete eigenvalues corresponding to the eigenvalues
%    2*pi^2 (simple), 5*pi^2 (double); 50*pi^2 (trifold)
% 3. associated discrete eigenvectors 
% n step length
% --- Matrix A -------------------------
e   = ones(n,1); I   = eye(n);
B   = spdiags([-e 2*e -e], -1:1, n, n);
A   = (kron(B,I) + kron(I,B))*(n+1)^2;
% --- Three eigenvalues of A -----------------
EW1A = 4*(n+1)^2*( sin(pi/(2*(n+1)))  )^2;
EW2A = 4*(n+1)^2*( sin(pi*2/(2*(n+1)))  )^2;
EW3A = 4*(n+1)^2*( sin(pi*7/(2*(n+1)))  )^2;
EW = [2*EW1A, EW1A + EW2A, EW1A + EW3A];
% --- Normed associated eigenvectors for zero bc - 
AUX1 = sin(pi*[1:n]/(n+1)).';
AUX2 = sin(2*pi*[1:n]/(n+1)).';
AUX3 = sin(5*pi*[1:n]/(n+1)).';
AUX4 = sin(7*pi*[1:n]/(n+1)).';
Y1= kron(AUX1,AUX1); Y1 = Y1/norm(Y1);
Y2A = kron(AUX1,AUX2); Y2B = kron(AUX2,AUX1);
Y2A = Y2A/norm(Y2A);   Y2B = Y2B/norm(Y2B);
Y2 = [Y2A,Y2B];
Y3A = kron(AUX1,AUX4); 
Y3B = kron(AUX4,AUX1); 
Y3C = kron(AUX3,AUX3); 
Y3A = Y3A/norm(Y3A); Y3B = Y3B/norm(Y3B); Y3C = Y3C/norm(Y3C);
Y3 = [Y3A,Y3B,Y3C];
EV1 = [Y1,Y2,Y3];
% --- Normed associated eigenvectors for mixed bc - 
AUX5 = [1,cos(pi*[1:n+1]/(n+1))].';
AUX6 = [1,cos(2*pi*[1:n+1]/(n+1))].';
AUX7 = [1,cos(5*pi*[1:n+1]/(n+1))].';
AUX8 = [1,cos(7*pi*[1:n+1]/(n+1))].';
Y1= kron(AUX1,AUX5); Y1 = Y1/norm(Y1);
Y2A = kron(AUX1,AUX6); Y2B = kron(AUX2,AUX5);
Y2A = Y2A/norm(Y2A);   Y2B = Y2B/norm(Y2B);
Y2 = [Y2A,Y2B];
Y3A = kron(AUX1,AUX8); 
Y3B = kron(AUX4,AUX5); 
Y3C = kron(AUX3,AUX7); 
Y3A = Y3A/norm(Y3A); Y3B = Y3B/norm(Y3B); Y3C = Y3C/norm(Y3C);
Y3 = [Y3A,Y3B,Y3C];
EV2 = [Y1,Y2,Y3];

