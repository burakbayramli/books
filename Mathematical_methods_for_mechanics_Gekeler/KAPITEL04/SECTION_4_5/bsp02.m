function Y = bsp02(X,flag,Parmeter);
% Orbit Problem, Bryson_Ho, p. 66
% flag = 1: Objective function
% flag = 2: Inequalities, also []
% flag = 3: Equalities 
% flag = 4: Gradient of objective function
% flag = 5: Gradient of inequalities, also []
% flag = 6: Gradient of equalities
% -------------------------------------------
% !! Gradient of f: R_n -> R_m is (m,n)-matrix !!
% -------------------------------------------
n = Parmeter(1); a = Parmeter(2); T_END = Parmeter(3);
A = T_END/n;
D = 0.5*ones(n+1,1);
E = zeros(n+1,n+1) + tril(ones(n+1,n+1),-1) + diag(D);
E(:,1) = D; E(1,1) = 0; 
E  = A*sparse(E);
GG = 20;
X1 = X(1:n+1); X2 = X(n+2:2*(n+1)); X3 = X(2*n+3:3*(n+1));
X4 = X(3*n+4:4*(n+1));  %Kontrolle
switch flag
case 1, Y = - GG*X1(n+1);
case 2, Y = 0;
case 3
   F1 =   X3.*X3./X1 - ones(n+1,1)./(X1.*X1) + a*sin(X4);
   F2 = - X2.*X3./X1 + a*cos(X4);
   Y  = [X1 - ones(n+1,1) - E*X2;
         X2 - E*F1;
         X3 - ones(n+1,1) - E*F2;
         X2(n+1);
         X3(n+1)*X3(n+1)*X1(n+1) - 1];

case 4, Y = zeros(1,4*(n+1)); Y(n+1) = - GG;
case 5, Y = 0;
case 6
   DF1_DX1 = - X3.*X3./(X1.*X1) + 2*ones(n+1,1)./(X1.*X1.*X1);
   DF1_DX2 =   zeros(n+1,1); DF1_DX3 =   2*X3./X1;
   DF1_DX4 =   a*cos(X4);    DF2_DX1 =   X2.*X3./(X1.*X1);
   DF2_DX2 = - X3./X1;       DF2_DX3 = - X2./X1;
   DF2_DX4 = - a*sin(X4);
   Y = [eye(n+1), -E, zeros(n+1,2*(n+1));
        -E*diag(DF1_DX1),eye(n+1), ...
        -E*diag(DF1_DX3),-E*diag(DF1_DX4);
        -E*diag(DF2_DX1), - E*diag(DF2_DX2), ...
        eye(n+1)-E*diag(DF2_DX3), -E*diag(DF2_DX4);
        zeros(2,4*(n+1))];
    Y(3*(n+1)+1,2*(n+1)) = 1;
    Y(3*(n+1)+2,(n+1)) = X3(n+1)*X3(n+1);
    Y(3*(n+1)+2,3*(n+1)) = 2*X3(n+1)*X1(n+1);
   Y = sparse(Y);
end
