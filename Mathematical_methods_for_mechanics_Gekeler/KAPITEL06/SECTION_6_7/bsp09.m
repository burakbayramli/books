function Y = bsp09(T,X,Parmeter);
% Differential system for THETA, D_THETA, und PHI
T1  = Parmeter(1);
T3  = Parmeter(2);
gl  = Parmeter(3);
m   = Parmeter(4);
tol = Parmeter(5);
D3  = Parmeter(6);
d3  = Parmeter(7);
T1  = m*T1; T3  = m*T3; mgl = m*gl;
Y = zeros(3,1);
Y(1) = X(2);
   ZZ = M3*sin(X(1))^2*(d3 - D3*cos(X(1)))- (d3 - D3*cos(X(1)))^2*cos(X(1));
Y(2) =  mgl*sin(X(1)) - ZZ/(T1*sin(X(1))^3);
Y(2) = Y(2)/T1;
Y(3) = (d3 - D3*cos(X(1)))/(T1*sin(X(1))*sin(X(1)));
