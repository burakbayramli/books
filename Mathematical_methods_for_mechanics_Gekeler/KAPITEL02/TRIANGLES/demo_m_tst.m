function demo_morley
% Test of Morley's triangle
% Comparison of both polynomials
% for individual values U in unit trianble,
% V in arbitrary triangle
clc, format compact, format short

COEFF = rand(1,6); COEFF = 10*COEFF;
% Choose arbitrary nondegenerate triangle, X-Y-coordinates
X = [0,0.7,-0.2]; Y = [0,0.5,1];
U = rand(1,2); % Argument of polynomial in unit triangle
if U(1) + U(2) > 1, U = (sqrt(2) - 1)*U; end
SUMU = U(1) + U(2) ;
U = [0,1/2];

X1 = X(1); X2 = X(2); X3 = X(3); Y1 = Y(1); Y2 = Y(2); Y3 = Y(3);
X21 = X2 - X1; X31 = X3 - X1; X32 = X3 - X2;
Y21 = Y2 - Y1; Y31 = Y3 - Y1; Y32 = Y3 - Y2;

V1 = X1 + X21*U(1) + X31*U(2); 
V2 = Y1 + Y21*U(1) + Y31*U(2); 
V = [V1,V2];

%Y1 = morley01(X,Y,V,COEFF);
%Y2 = morley02(X,Y,V,COEFF);
%Y3 = morley03(X,Y,V,COEFF);
%Y4 = morley04(X,Y,U,COEFF);
Y5 = morley05(X,Y,V,COEFF)


C4 = COEFF(6)

