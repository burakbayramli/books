function test03
% Rayleigh-Zahl und Prandtl-Zahl
format short e
%
% Beispiel 3 bzw. 7
g = 9.81; BETA = 0.21e-3; DIFF_T = 40; L = 3;
NU = 1.49e-2; LAMBDA = NU;
RA = g*BETA*DIFF_T*L^3/(NU*LAMBDA)
