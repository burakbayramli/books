function Y = bsp07(X,flag,Parmeter);
% Example of Hartl et al. p. 208
% flag = 1: Objective function
% flag = 2: Inequalities, also []
% flag = 3: Equalities 
% flag = 4: Gradient of objective function
% flag = 5: Gradient of inequalities, also []
% flag = 6: Gradient of equalities
% -------------------------------------------
% !! Gradient of f: R_n -> R_m is (m,n)-matrix !!
% -------------------------------------------
n = Parmeter(1); alfa = Parmeter(2); T_END = Parmeter(3);
A = T_END/n; FAKTOR = ones(1,n+1);
FAKTOR(1) = 0.5; FAKTOR(n+1) = 0.5;
D      = 0.5*ones(n+1,1);
E      = zeros(n+1,n+1) + tril(ones(n+1,n+1),-1) + diag(D);
E(:,1) = D; E(1,1) = 0;
FAKTOR = A*FAKTOR;
E      = A*sparse(E);
X1  = X(1:n+1); X2  = X(n+2:2*n+2); U = X(2*n+3:3*(n+1));
switch flag
case 1, Y = 2*FAKTOR*X1;
case 2, Y   = [X1 - alfa; U + 2; 3 - U];
case 3, Y = [X1 - 2 - E*X2; X2 - E*U];
case 4, Y = [2*FAKTOR, zeros(1,n+1), zeros(1,n+1)];
case 5
   Y = [ eye(n+1),       zeros(n+1,n+1),  zeros(n+1,n+1);
         zeros(n+1,n+1), zeros(n+1,n+1),  eye(n+1);
         zeros(n+1,n+1), zeros(n+1,n+1), -eye(n+1)];
   Y = sparse(Y);
case 6
   Y = [eye(n+1), -E, zeros(n+1,n+1);
        zeros(n+1,n+1), eye(n+1), -E];
end
