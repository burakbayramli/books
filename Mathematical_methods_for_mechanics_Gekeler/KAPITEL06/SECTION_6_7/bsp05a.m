function Y = bsp05a(T,X,Parmeter);
% Differential system for top
% in Euler angles, right side of DAE system
T1  = Parmeter(1);
T3  = Parmeter(2);
m   = Parmeter(3);
gl  = Parmeter(4);
T1  = m*T1; T3  = m*T3; mgl = m*gl;
U   = X(1:3); V = X(4:6); Y = zeros(6,1);
Y(1:3) = V;
Y(4:6) = [2*V(1)*V(2)*(T1-T3)*cos(U(2))-V(2)*V(3)*T3;
          V(1)^2*(T3 - T1)*cos(U(2))+V(1)*V(3)*T3-mgl*sin(U(2));
         -V(1)*V(2)*T3];
         Y(4:6) = - Y(4:6)*sin(U(2));
