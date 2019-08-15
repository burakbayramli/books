function Y = bsp03a(X,flag,Parmeter);
% Zermelo's Problem, cf. Bryson_Ho, p. 77
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
n  = Parmeter(1); S = Parmeter(2); A1 = Parmeter(3);
A2 = Parmeter(4); B1 = Parmeter(5); B2 = Parmeter(6);
T_END = Parmeter(7);
X1 = X(1:n+1); X2 = X(n+2:2*(n+1));
X3 = X(2*n+3:3*(n+1));  % Kontrolle
X4 = X(3*n+4);          % Zeit
A = T_END/n; 
D = 0.5*ones(n+1,1);
E = zeros(n+1,n+1) + tril(ones(n+1,n+1),-1) + diag(D);
E(:,1) = D; E(1,1) = 0; 
E = A*sparse(E);
GG = 10;
switch flag
case 1, Y = GG*X4;
case 2, Y = 0;
case 3,
   [V1,V2] = fluss(X,1,Parmeter);
   Y = [X1 - A1*ones(n+1,1) - X4*E*(S*cos(X3) + V1);
        X2 - A2*ones(n+1,1) - X4*E*(S*sin(X3) + V2);
        X1(n+1) - B1;
        X2(n+1) - B2];
case 4, Y = derivative(@bsp03,X,1,hh,Parmeter);
case 5, Y = 0;
case 6, Y = derivative(@bsp03,X,3,hh,Parmeter);
        Y = sparse(Y);
end
