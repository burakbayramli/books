function Y = bsp02b(X,flag);
% minimal surface
% as BSP02A.M but gradient of F approximated
% flag = 1: Function F
% flag = 2: Gradient of F
% flag = 3: Boundary condition
% flag = 4: Gradient of boundary condition

global r0 r1
if flag == 1
   Y = [X(2); (1+X(2)^2)/X(1)];
end
if flag == 2
   DIFF = 1e-2;
   U0 = X; V0 = fun(U0);
   U1 = X + DIFF*[1;0]; V1 = fun(U1);
   DFDX = (V1 - V0)/DIFF;
   U2 = X + DIFF*[0;1]; V2 = fun(U2);
   DFDY = (V2 - V0)/DIFF;
   Y = [0, 1; DFDX, DFDY];
end
if flag == 3
   V = X(3:4);
   Y = [X(1) - r0;
        V(1) - r1];
end
if flag == 4
   D1 = [1 0; 0 0]; D2 = [0 0; 1 0];
   Y = [D1; D2];
end
