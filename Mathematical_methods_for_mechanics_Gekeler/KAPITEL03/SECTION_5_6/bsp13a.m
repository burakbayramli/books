function Y = bsp13a(X,flag,Parmeter);
% Himmelblau, p. 404
% flag = 1: Objective function
% flag = 2: Inequalities
% flag = 3: Equalities
% flag = 4: Gradient of objective function
% flag = 5: Gradient of inequalities
% flag = 6: Gradient of equalities
% -------------------------------------------
% Gradient of f: R_n -> R_m is (m,n)-matrix
% -------------------------------------------
hh = 1E-5; % increment for calculation of derivative

u = [-15 -27 -36 -18 -12];
v = [4  8  10  6  2];
A = [ 30  -20  -10  32  -10;
     -20   39   -6 -31   32;
     -10   -6   10  -6  -10;
      32  -31   -6  39  -20;
     -10   32  -10 -20   30];
B = [-16   2   0   1   0;
       0  -2   0  0.4  2;
    -3.5  0   2   0   0;
       0  -2   0  -4  -1;
       0  -9  -2   1 -2.8;
       2   0  -4   0   0;
      -1  -1  -1  -1  -1;
      -1  -2  -3  -2  -1;
       1   2   3   4   5;
       1   1   1   1   1];
switch flag
case 1, Y = u*X + v*(X.^3) + X'*A*X;
case 2
   b =  [40; 2; 0.25; 4; 4; 1; 40; 60; -5; -1];
   g = B*X + b;
   Y = [g; X];
case 3, Y = 0;
case 4, Y = derivative(@bsp13,X,1,hh,Parmeter);
case 5, Y = derivative(@bsp13,X,2,hh,Parmeter);
case 6, Y = 0;
end
