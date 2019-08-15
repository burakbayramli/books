function Y = bsp06a(X,flag,Parmeter);
% Example of Hartl et al. p. 207
% flag = 1: Objective function
% flag = 2: Inequalities, also []
% flag = 3: Equalities 
% flag = 4: Gradient of objective function
% flag = 5: Gradient of inequalities, also []
% flag = 6: Gradient of equalities
% -------------------------------------------
% !! Gradient of f: R_n -> R_m is (m,n)-matrix !!
% -------------------------------------------
hh = 1E-5; % increment for calculation of derivative
n = Parmeter(1); r = Parmeter(2); T_END = Parmeter(3);
A = T_END/n; FAKTOR = ones(1,n+1);
FAKTOR(1) = 0.5; FAKTOR(n+1) = 0.5;
D = 0.5*ones(n+1,1);
E = zeros(n+1,n+1) + tril(ones(n+1,n+1),-1) + diag(D);
E(:,1)= D; E(1,1) = 0;
FAKTOR = A*FAKTOR;
E      = A*sparse(E);
XX  = X(1:n+1); U   = X(n+2:2*(n+1));
AUX = exp(-r*linspace(0,3,n+1));
switch flag
case 1, Y = FAKTOR*(AUX'.*U);
case 2,
   Y  = [XX - 1 + (linspace(0,3,n+1)' - 2).^2;
         U;
         3 - U];
case 3, Y = XX - E*U;
case 4, Y = derivative(@bsp06,X,1,hh,Parmeter);
case 5, Y = derivative(@bsp06,X,2,hh,Parmeter);
        Y = sparse(Y);
case 6, Y = derivative(@bsp06,X,3,hh,Parmeter);
        Y = sparse(Y);
end
