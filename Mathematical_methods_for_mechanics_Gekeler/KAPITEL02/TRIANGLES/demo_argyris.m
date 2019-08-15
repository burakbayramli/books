function demo_argyris
% Test of Argyris triangle
% Comparison of polynomials, 
% for individual values U
clc

COEFF = rand(1,21); COEFF = 10*COEFF;
% Choose arbitrary nondegenerate triangle, X-Y-coordinates
X = [0,0.7,-0.2]; Y = [0,0.5,1];
U = rand(1,2); % Argument of polynomial in unit triangle
if U(1) + U(2) > 1, U = (sqrt(2) - 1)*U; end
SUMU = U(1) + U(2) 

% -- Transformation of unit triangle to general triangle
X21 = X(2) - X(1); X31 = X(3) - X(1); 
Y21 = Y(2) - Y(1); Y31 = Y(3) - Y(1); 
V(1) = X(1) + X21*U(1) + X31*U(2); 
V(2) = Y(1) + Y21*U(1) + Y31*U(2); 
DET = X21*Y31 - X31*Y21
Y1 = argyris01(X,Y,V,COEFF); % direct
Y2 = argyris02(X,Y,V,COEFF); % with area coordinates
Y3 = argyris03(X,Y,V,COEFF); % cyclic representation

Y1_Y2_Y3 = [Y1,Y2,Y3]

