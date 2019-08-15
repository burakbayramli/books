function W = gauss_t5(X,Y,Q,DEGREE)
%function W = gauss_t5('func',X,Y)

% X,Y Data of corners of triangle
% Q data of polynomial ordered as POLYNOM.M
% Gauss-Integration over arbitrary triangle
% DEGREE = 5;
% Origin must be moved into center of triangle

if DEGREE > 5, W = NAN; return, end
KOEFF_NUMBER = (DEGREE+1)*(DEGREE+2)/2;
AUX = 21 - KOEFF_NUMBER;
P = [Q,zeros(1,AUX)]; % coefficients of polynomial

X21 = X(2) - X(1); X31 = X(3) - X(1);
Y21 = Y(2) - Y(1); Y31 = Y(3) - Y(1);

F  = (X21*Y31 - X31*Y21)/2;
AUX1 = [X(1);X21;X31]; AUX2 = [Y(1);Y21;Y31];
G1 = (155 + sqrt(15))/1200; G2 = (155 - sqrt(15))/1200;
GAMMA = F*[0.225, G1, G1, G1, G2, G2, G2];
a = (6 +   sqrt(15))/21; b = (9 - 2*sqrt(15))/21;
c = (6 -   sqrt(15))/21; d = (9 + 2*sqrt(15))/21;
ZETA = [1, 1/3, 1/3; 1, a, a; 1, b, a;
        1,   a,   b; 1, c, c; 1, d, c; 1, c, d];

U = ZETA*AUX1; V = ZETA*AUX2;
Z = zeros(7,1);
for I = 1:7
   Z(I) = polynom(U(I),V(I),P);
   % replace by numerical integrand 
   % Z(I) = feval('func',U(I),V(I));
end     
W = GAMMA*Z;
