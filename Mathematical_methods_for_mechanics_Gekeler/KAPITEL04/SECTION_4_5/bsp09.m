function Y = bsp09(X,flag,Parmeter);
% Brachistochrone Problem, cf. Bryson, p. 119
% flag = 1: Objective function
% flag = 2: Inequalities, also []
% flag = 3: Equalities 
% flag = 4: Gradient of objective function
% flag = 5: Gradient of inequalities, also []
% flag = 6: Gradient of equalities
% -------------------------------------------
% !! Gradient of f: R_n -> R_m is (m,n)-matrix !!
% -------------------------------------------
n  = Parmeter(1); g  = Parmeter(2); B1 = Parmeter(3);
T_END = Parmeter(4); C = Parmeter(5);
X1 = X(1:n+1); X2 = X(n+2:2*(n+1));
X3 = X(2*n+3:3*(n+1));  % Kontrolle
X4 = X(3*n+4);          % Zeit
AUX     = sqrt(2*g*X2);
J       = find(AUX == 0);
AUX1    = AUX;
AUX1(J) = 1;
AUX1    = ones(n+1,1)./AUX1;
AUX1(J) = 0;

A           = T_END/n;
FAKTOR      = ones(1,n+1);
FAKTOR(1)   = 0.5;
FAKTOR(n+1) = 0.5;
D           = 0.5*ones(n+1,1);
E           = zeros(n+1,n+1) + tril(ones(n+1,n+1),-1) + diag(D);
E(:,1)      = D;
E(1,1)      = 0;
FAKTOR      = A*FAKTOR;
E           = A*sparse(E);
GG          = 1;
switch flag
case 1, Y =  GG*X4;
case 2, Y = C + 0.5*X1 - X2;
case 3
   Y = [X1 - X4*E*(AUX.*cos(X3));
        X2 - X4*E*(AUX.*sin(X3));
        X1(n+1) - B1];
case 4, Y = zeros(1,3*n+4); Y(3*n+4) = GG;
case 5
   Y = [ 0.5*eye(n+1), -eye(n+1), zeros(n+1,n+2)];
   Y = sparse(Y);
case 6
   Y1 = [eye(n+1),-X4*E*diag(g*cos(X3).*AUX1), ...
         X4*E*diag(AUX.*sin(X3)), - E*(AUX.*cos(X3))];
   Y2 = [zeros(n+1,n+1), eye(n+1)- X4*E*diag(g*sin(X3).*AUX1), ...
         -X4*E*diag(AUX.*cos(X3)),-E*(AUX.*sin(X3))];
   Y =  [Y1;Y2;zeros(1,3*n+4)];
   Y(2*(n+1)+1,n+1)     = 1;
   Y = sparse(Y);
end
