function Z = polynom(X,Y,P)
% evaluates a two-dimensional polynomial of degree 5 at point (X,Y)
% P : <= 10 coeff. P(1) + P(2)X + P(3)Y
% + P(4)X**2 + P(5)XY + P(6)Y**2 + ....

X2 = X*X; X3 = X2*X; X4 = X3*X; X5 = X4*X;
Y2 = Y*Y; Y3 = Y2*Y; Y4 = Y3*Y; Y5 = Y4*Y;
Z1 = P(1) + P(2)*X + P(3)*Y + P(4)*X2 + P(5)*X*Y + P(6)*Y2;
Z2 = P(7)*X3 + P(8)*X2*Y + P(9)*X*Y2 + P(10)*Y3   ;
Z3 = P(11)*X4 + P(12)*X3*Y + P(13)*X2*Y2 + P(14)*X*Y3 + P(15)*Y4;
Z4 = P(16)*X5 + P(17)*X4*Y + P(18)*X3*Y2 + P(19)*X2*Y3 + P(20)*X*Y4 + P(21)*Y5;
Z = Z1 + Z2 + Z3 + Z4;
