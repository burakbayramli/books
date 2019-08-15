function Y = bsp02a(X,flag);
% minimal surface
% flag = 1: Function F
% flag = 2: Gradient of F
% flag = 3: Boundary condition
% flag = 4: Gradient of boundary condition

global r0 r1
if flag == 1
   Y = [X(2); (1+X(2)^2)/X(1)];
end
if flag == 2
   Y = [0, 1; (-1-X(2)^2)/(X(1)^2), 2*X(2)/X(1)];
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