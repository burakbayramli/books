function Y = bsp04a(X,flag,Parmeter)
% Servo Problem, Burges-Graham p. 281
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
n  = Parmeter(1); a = Parmeter(2); omga = Parmeter(3);
A1 = Parmeter(4); A2 = Parmeter(5);
GG = Parmeter(6); FF = Parmeter(7);
A  = 1/n; 
D = 0.5*ones(n+1,1);
E = zeros(n+1,n+1) + tril(ones(n+1,n+1),-1) + diag(D);
E(:,1) = D; E(1,1) = 0;
E      = A*sparse(E);
X1 = X(1:n+1); X2 = X(n+2:2*(n+1));
X3 = X(2*n+3:3*(n+1));  % Kontrolle
X4 = X(3*n+4);          % Zeit
switch flag
case 1, Y =  GG*X4;
case 2, Y = [X3 + 1;1 - X3];
case 3
   Y1 = X1 - A1 - X4*E*X2;
   Y2 = X2 - A2 - X4*E*(X3 - omga*X1 - a*X2);
   Y = [Y1;Y2;X1(n+1);X2(n+1)]; 
   Y = FF*Y;
case 4, Y = derivative(@bsp04,X,1,hh,Parmeter);
case 5, Y = derivative(@bsp04,X,2,hh,Parmeter);
        Y = sparse(Y);
case 6, Y = derivative(@bsp04,X,3,hh,Parmeter);
        Y = sparse(Y);
end
