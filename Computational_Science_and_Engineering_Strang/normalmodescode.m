%2.2  normalmodescode.m

% inputs M, K, uzero, vzero, t
[vectors,values] = eig(K,M); eigen = diag(values); % solve Kx = (lambda)Mx
A = vectors\uzero; B = (vectors*sqrt(values))\vzero;
coeffs = A.*cos(t*sqrt(eigen)) + B.*sin(t*sqrt(eigen));
u = vectors*coeffs;  % solution at time t to Mu'' + Ku = 0 

