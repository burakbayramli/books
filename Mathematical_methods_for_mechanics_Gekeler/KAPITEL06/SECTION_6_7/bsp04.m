function Y = bsp04(T,X,Parmeter);
% DGl for THETA
T1  = Parmeter(1); T3  = Parmeter(2);
gl  = Parmeter(3); m   = Parmeter(4);
tol = Parmeter(5); d3  = Parmeter(6);
D3  = Parmeter(7);
T1  = m*T1; T3  = m*T3;
Y = zeros(2,1);
Y(1) = X(2);
if abs(X(1)) > tol & abs(X(1)) < pi- tol
   ZZ = D3*sin(X(1))^2*(d3 - D3*cos(X(1)))- (d3-D3*cos(X(1)))^2*cos(X(1));
   Y(2) =  m*gl*sin(X(1)) - ZZ/(T1*sin(X(1))^3);
   Y(2) = Y(2)/T1;
else
   Y(2) = 0;
end
