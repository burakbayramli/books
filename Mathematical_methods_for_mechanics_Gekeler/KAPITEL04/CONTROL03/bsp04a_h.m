function Z = bsp04a_h(flag,X,Y);
% Dyer-McReynolds, p. 73
% flag = 1: Objective function with add. boundary cond.
% flag = 2; Initial cond. for x
% flag = 3: Terminal cond. for y
% flag = 4: DH/DU
% flag = 5: Terminal data direct

global kappa n h U
GG = 80; PP = 80; % additional weights
switch flag
case 1
   Z = (X(2,n+1) - h)^2;
%  Z = (X(2,n+1) - h)^2 + X(4,n+1)^2;
%  Z = - X(3,n+1) + (X(2,n+1) - h)^2 + X(4,n+1)^2;
case 2,  Z = [0; 0; 0; 0];
case 3
   Z = [0; 2*(X(2,n+1) - h); 0; 0];
%  Z = [0; 2*(X(2,n+1) - h); 0; 2*X(4,n+1)];
%  Z = [0; 2*(X(2,n+1) - h); -1; 2*X(4,n+1)];
case 4
   Z = kappa*(Y(4,:).*cos(U)  - Y(3,:).*sin(U));
case 5
   Z1 = X(2,n+1) - h;
   Z2 = X(4,n+1);
   Z  = [Z1; Z2];
end
