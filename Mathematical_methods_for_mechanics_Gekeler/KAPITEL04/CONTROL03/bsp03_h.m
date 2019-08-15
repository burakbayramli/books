function Z = bsp03_h(flag,Y,Y1,Y2);
% Dyer-McReynolds, p. 73
% flag = 1: Objective function with trapezoidal rule
% flag = 2; DH/DU completely
% flag = 3: Additional boundary conditions
% flag = 4: Additional boundary conditions
% flag = 5: Initial cond. for x, terminal cond. for y
% flag = 6: Terminal cond. for additional boundary cond.

global kappa n U X
GG = 10;
switch flag
case 1, Z = - GG*X(1,n+1);
case 2, Z = kappa*(Y(2,:).*cos(U)  - Y(3,:).*sin(U));
case 3
   Z1 = kappa*(Y1(2,:).*cos(U) - Y1(3,:).*sin(U));
   Z2 = kappa*(Y2(2,:).*cos(U) - Y2(3,:).*sin(U));
   Z  = [Z1;Z2];
case 4
   Z1 = X(2,n+1)^2;
   Z2 = (X(1,n+1)*X(3,n+1)^2 - 1)^2;
   Z  = [Z1; Z2];
case 5
   Z1 = [ 1; 0; 1];   % x(0) = a
   Z2 = [-1; 0; 0];   % y(T) = p_x(T)
   Z  = [Z1, GG*Z2];
case 6
   AUX = 2*(X(1,n+1)*X(3,n+1)^2 - 1);
   Z1  = [0; 2*X(2,n+1); 0];                        % q_1
   Z2  = AUX*[X(3,n+1)^2; 0; 2*X(1,n+1)*X(3,n+1)];  % q_2
   Z   = GG*[Z1, Z2];
case 7
   Z1 = X(2,n+1);
   Z2 = X(1,n+1)*X(3,n+1)^2 - 1;
   Z  = [Z1; Z2];
end
