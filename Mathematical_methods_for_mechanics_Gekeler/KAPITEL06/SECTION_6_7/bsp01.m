function Y = bsp01(T,X,Parmeter);
% Differential system for Euler angles of top
T1  = Parmeter(1); T3  = Parmeter(2);
m  = Parmeter(3); gl   = Parmeter(4);
tol = Parmeter(5);
T1  = m*T1; T3  = m*T3; mgl = m*gl;
U = X(1:3); V = X(4:6); Y = zeros(6,1);
Y(1:3) = V;
if abs(U(2)) > tol & abs(U(2)) < pi- tol
   INVA = [T3, 0, -T3*cos(U(2));
           0, T3*sin(U(2))^2, 0;
           -T3*cos(U(2)), 0, T1*sin(U(2))^2+T3*cos(U(2))^2];
   INVA = INVA/(T1*T3*sin(U(2))^2);
   B = [2*V(1)*V(2)*(T1-T3)*sin(U(2))*cos(U(2))-V(2)*V(3)*T3*sin(U(2));
      (T3-T1)*V(1)^2*sin(U(2))*cos(U(2))+T3*V(1)*V(3)*sin(U(2))-m*gl*sin(U(2));
      -V(1)*V(2)*T3*sin(U(2))];
   Y(4:6) = -INVA*B;
else
   Y(4:6) = [0;0;0];
end
