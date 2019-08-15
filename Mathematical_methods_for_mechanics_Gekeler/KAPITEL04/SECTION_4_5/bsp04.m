function Y = bsp04(X,flag,Parmeter)
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
case 4, Y = zeros(1,3*n+4); Y(3*n+4) = 1;
        Y = GG*Y;
case 5,
   Y = [zeros(n+1,2*(n+1)), eye(n+1), zeros(n+1,1);
        zeros(n+1,2*(n+1)), -eye(n+1), zeros(n+1,1)];
case 6
   Y1 = [eye(n+1),  -X4*E,           zeros(n+1,n+1), -E*X2];
   Y2 = [omga*X4*E, eye(n+1)+a*X4*E, -X4*E,          -E*(X3-omga*X1-a*X2)];
   YA = [Y1;Y2];
   YB = zeros(2,3*n+4);
   YB(1,n+1)     = 1;
   YB(2,2*(n+1)) = 1;  
   Y = [YA;YB];
   Y = sparse(Y);
   Y = FF*Y;
end
