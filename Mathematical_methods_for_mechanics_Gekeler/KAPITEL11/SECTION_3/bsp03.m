function F_WERT = bsp03(flag,T,X,Y,Z,parmtr3);
% Doppelpendel ------
% Argument Z wird hier nicht gebraucht
% flag = 1: Funktion f
% flag = 2: Zwangsbedingung g
% flag = 3: Gradient von g
% flag = 4: t-Ableitung von g
% flag = 5: Massenmatrix M
% -------------------------------------------
% Gradient von f: R_n -> R_m ist (m,n)-Matrix
% -------------------------------------------
g = 9.81;
g = parmtr3(1); m = parmtr3(2); L = parmtr3(3);
switch flag
case 1
   F_WERT = [0; -m*g; 0; -m*g];
case 2
   F_WERT = [X(1)*X(1) + X(2)*X(2) - L*L;
             (X(3)-X(1))^2 + (X(4)-X(2))^2 - L*L];
case 3
   F_WERT = [2*X(1), 2*X(2), 0, 0;
             -2*(X(3)-X(1)), -2*(X(4)-X(2)),...
              2*(X(3)-X(1)), 2*(X(4)-X(2))];
case 4
   F_WERT = [0;0];
case 5
   F_WERT = [m, 0; 0, m];
   F_WERT = [F_WERT, zeros(2,2); zeros(2,2), F_WERT];
end
