function test03
format short
A = [1 2 3; 4 5 6; 3 3 1];
A(2:3) = 0;
A;
% WATER, PR = NU/LAMBDA
nu = 1.004E-6; lamda = 1.40E-7;
PR = nu/lamda;

% Luft
nu = 1.33E-5; lamda = 2.00E-5;
PR = nu/lamda