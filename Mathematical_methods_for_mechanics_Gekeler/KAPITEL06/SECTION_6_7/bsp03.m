function Y = bsp03(T,X,Parmeter);
% Differential system for THETA, D_THETA, and PHI
T1   = Parmeter(1); T3  = Parmeter(2);
m    = Parmeter(3); gl  = Parmeter(4);
d3   = Parmeter(5); D3  = Parmeter(6);
T1   = m*T1; T3  = m*T3; Y   = zeros(3,1);
Y(1) = X(2);
if d3*d3 ~= D3*D3
   ZZ   = D3*sin(X(1))^2*(d3 - D3*cos(X(1))) ...
          - (d3 - D3*cos(X(1)))^2*cos(X(1));
   Y(2) = m*gl*sin(X(1)) - ZZ/(T1*sin(X(1))^3);
   Y(2) = Y(2)/T1;
   Y(3) = (d3 - D3*cos(X(1)))/(T1*sin(X(1))^2);
end
if d3 == D3
   Y(2) = m*gl*sin(X(1))...
          - D3^2*sin(X(1))/(T1*(1 + cos(X(1)))^2);
   Y(2) = Y(2)/T1;
   Y(3) = D3/(T1*(1 + cos(X(1))));
end
if d3 == - D3
   Y(2) = m*gl*sin(X(1))...
          + D3^2*sin(X(1))/(T1*(1 - cos(X(1)))^2);
   Y(2) = Y(2)/T1;
   Y(3) = D3/(T1*(1 - cos(X(1))));
end
