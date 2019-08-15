function cr_aux
% Hilfsfile fuer Crouzeix-Raviart-element

clc
A = [2, 1, 0; 2, 1, 1; 2, 0, 1]/2;
B = [1, -1, 1; 0, 2, -2; -2, 2, 0];
C = A*B;  % o.k.
AA1 = [0, 0, 0; 0, 1, 0; 0, 0, 0]; %/2
AA2 = [0, 0, 0; 0, 0, 1; 0, 1, 0]; % /4
AA3 = [0, 0, 0; 0, 0, 0; 0, 0, 1]; %/2
AA4 = [12, 4, 4; 4, 2, 1; 4, 1, 2]; %24
aa  = [3, 1, 1]; % /6

A1 = B'*AA1*B 
A2 = B'*AA2*B
A3 = B'*AA3*B
A4 = B'*AA4*B
a = aa*B
