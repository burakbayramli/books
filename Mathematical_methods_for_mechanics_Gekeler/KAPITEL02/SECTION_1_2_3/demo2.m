function demo2
% Test of integration rules over arbitrary triangle T
% X,Y coordinates of Corners of triangle
% DEGREE : degree of polynomial
% P: Coefficients of polynomial (random)
% ordered as in POLYNOM.M
% bell.m: DEGREE arbitrary (with SYMBOLIC MATH.)
% gauss_t5.m: DEGREE = 5 (Gauss without SYMBOLIC MATH.)
% polynomials of degree <= 5 are filled up with zeros

clc, format short e, format compact
% Choose coordinates of arbitrary triangle
% Determinant must be positive:
% -- Choose Data: ------------- 
X = [0; 0.7; 0.6]; Y = [0;0;1];
DEGREE = 5; % 
% -----------------------------
KOEFF_NUMBER = (DEGREE+1)*(DEGREE+2)/2
% random values of polynomial of degree 5
P = rand(1,KOEFF_NUMBER);  
BELL = bell(X,Y,P,DEGREE)
if DEGREE <= 5
   GAUSS = gauss_t5(X,Y,P,DEGREE);
   DIFF_BELL_GAUSS = BELL - GAUSS
end
