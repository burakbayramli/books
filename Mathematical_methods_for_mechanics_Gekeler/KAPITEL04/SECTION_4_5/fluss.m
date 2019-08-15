function [V1,V2] = fluss(X,flag,Parmeter);
% Calculates current flow for Zermelo's problem
n  = Parmeter(1); S  = Parmeter(2); fac = Parmeter(8);
S  = fac*S;
X1 = X(1:n+1); X2 = X(n+2:2*(n+1));
X3 = X(2*n+3:3*(n+1));  % Control
X4 = X(3*n+4);          % Time
switch flag
case 1, V1 = - S*X2; V2 =  zeros(n+1,1);
case 2, V1 = [zeros(n+1,n+1), -S*eye(n+1), zeros(n+1,n+2)];
        V2 = zeros(n+1,3*n+4);
end
