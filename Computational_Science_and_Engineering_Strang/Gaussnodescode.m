%5.4  Gaussnodescode.m

b = 1/2*sqrt(1 - (2*(1:n)).^(-2)); % off-diagonal entries of Legendre's L
L = diag(b,1) + diag(b,-1);        % symmetric tridiagonal L of size n + 1
[V,X] = eig(L);                    % eigenvectors in V, eigenvalues in X
x = diag(X); [x,j] = sort(x);      % Gauss nodes x_j are eigenvalues of L
w = 2*V (1,j).^2;                  % the weights w_j are all positive
I = w*f(x);                        % exact quadrature for degree 2n + 1
