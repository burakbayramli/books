function Y = bsp04a(X,flag,Parmeter);
% Nonlinear Poisson equation
% - Delta(u0 + w) - f(mu0 + mu,u0 + w) = 0; UU'*w = 0;
% in unit square with u = 0 on entire boundary
% flag = 1: Funktion f(mu,x) = mu*sin(x)
% flag = 2: Gradient_x of f
% flag = 3: Right side for continuation
% flag = 4: Matrix B for bifurcation equation
% -------------------------------------------
% Gradient of f: R_n -> R_m is (m,n)-matrix
% A is discretization of negative Laplacian
% by finite differences
% --Parameters ---------------------------------
MU = Parmeter(1); NU = Parmeter(2);  SIGN = Parmeter(3);
n  = Parmeter(4);
% ----------------------------------
global A
switch flag
case 1, Y = A*X - MU*sin(X);
case 2,
   D  = cos(X);
   Y  = A - MU*diag(D); Y = sparse(Y);
case 3
     Y = MU*(sin(X) - X);
case 4, Y = eye(n*n);
end
