function Y = test1(X,flag)
% Testfunktion zur Minimierung, vgl. Spellucci, S.117
% flag = 1: Funktion F
% flag = 2: Gradient von F (Zeilenvektor)

R = sqrt(1 + X(1)*X(1) + X(2)*X(2));
if flag == 1
   Y = 1.1*X(1)*X(1) + 1.2*X(2)*X(2)...
       -2*X(1)*X(2) - 7*X(1) - 3*X(2) + R;
end
if flag == 2
   Y    = zeros(1,2);
   Y(1) = 2.2*X(1) - 2*X(2) + X(1)/R - 7;
   Y(2) = - 2*X(1) + 2.4*X(2)+ X(2)/R - 3;
end
