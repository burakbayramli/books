function INTEGRAL = prs1(X,Y,r,s)
% Calculates integrals of x^r*y^s  for cartesian coordinates x,y 
% over arbitrary triangle T, uses SYMBOLIC MATLAB TOOLBOX 
% X,Y Coordinates of triangle

syms xi eta
SX  = sum(X)/3; SY  = sum(Y)/3;
U   = X - SX;   V   = Y - SY;
U = X; V = Y;
U21 = U(2) - U(1); V21 = V(2) - V(1);
U31 = U(3) - U(1); V31 = V(3) - V(1);
DET = U21*V31 - U31*V21;
AUX1 = U(1) + U21*xi + U31*eta;
AUX2 = V(1) + V21*xi + V31*eta;
INTEGRAND = AUX1^r*AUX2^s;
INTEGRAL = int(INTEGRAND,xi,0,1-eta);
INTEGRAL = simplify(INTEGRAL);
INTEGRAL = int(INTEGRAL,eta,0,1);
INTEGRAL = DET*vpa(INTEGRAL);
