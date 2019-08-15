% ORBIT.M, GEKELER: OPTIMIERUNG --------------------------
% Rechte Seite der Dgl. und Gradienten fÅr Orbit-Problem
% vgl. Bryson-Ho, S. 66 ff.

function Y = thrust(X,t,flag);
global g m md r0 S
N =  sqrt(X(5)*X(5) + X(6)*X(6));
C = X(6)/N;
D = X(5)/N;
if flag == 1
   Y    = zeros(6,1);
   Y(1) = X(2);
   Y(2) = X(3)*X(3)/X(1) - g/(X(2)*X(2)) + S*D/(m - md*t);
   Y(3) = S*C/(m - md*t) - X(2)*X(3)/X(1);
   Y(4) = X(3)*X(3)*X(5)/(X(1)*X(1))- 2*g*X(5)/X(1)^3 ...
          - X(2)*X(3)*X(5)/(X(1)*X(1));
   Y(5) = X(3)*X(5)/X(1) - X(4);
   Y(6) = X(2)*X(6)/X(1) - 2*X(3)*X(5)/X(1);
end
if flag == 2
   Y = sparse(6,6);
   Y(1,2) = 1;
   Y(2,1) = - X(3)*X(3)/(X(1)*X(1)) - 2*g/X(1)^3;
   Y(2,3) = 2*X(3)/X(1);
   Y(3,:) = [X(2)*X(3)/(X(1)*X(1)),-X(3)/X(1),-X(2)/X(1),0,0,0 ];
   Y(4,1) = -2*X(3)*X(3)*X(5)/X(1)^3+6*g/X(1)^4+X(2)*X(3)*X(5)/X(1)^3;
   Y(4,2) = - X(3)*X(5)/X(1)^2;
   Y(4,3) = 2*X(3)*X(5)/X(1)^2 + X(2)*X(6)/X(1)^2;
   Y(4,4) = 0;
   Y(4,5) = X(3)^2/X(1)^2 - 2*g/X(1)^3;
   Y(4,6) = - X(2)*X(3)/X(1)^2;
   Y(5,1) = - X(3)*X(5)/X(1)^2;
   Y(5,2) = 0;
   Y(5,3) = X(5)/X(1);
   Y(5,4) = -1;
   Y(5,5) = X(3)/X(1);
   Y(6,1) = 2*X(3)*X(5)/X(1)^2 - X(2)*X(6)/X(1)^2;
   Y(6,2) = X(6)/X(1);
   Y(6,3) = - 2*X(5)/X(1);
   Y(6,4) = 0;
   Y(6,5) = - 2*X(3)/X(1);
   Y(6,6) = X(2)/X(1);
end
   D1  = [1 0 0 0 0 0;
          0 1 0 0 0 0;
          0 0 1 0 0 0];
   D2  = [0 1 0 0 0 0;
          0 0 1 0 0 0;
          0 0 0 1 0 0];
   A = [r0; 0; sqrt(g/r0)];
   B = [0; sqrt(g/X(1)); 1];
   BD = sparse(6,6);
   BD(2,1) = - sqrt(g/X(1)^3)/2;
if flag == 3                            % Randbedingung
   Y  = [D1*X(1:6)- A; D2*X(7:12) - B];
end
if flag == 4                   % Gradient der Randbedingung
   D1 = [D1; zeros(3,6)];
   D2 = [zeros(3,6); D2] - BD;
   Y = [D1, D2];
end
