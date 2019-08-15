function test
% formula of Holand and Bell
clc
p = 3; q = 5; r = 7;
syms xi eta 
func = xi^p*eta^q;
F1 = int(func,xi,0,1-eta);
F1 = simplify(F1);
F2 = int(F1,eta,0,1);
F3 = factorial(p)*factorial(q)/factorial(p+q+2);
DIFF1 = F2 - F3
zeta = 1-xi -eta;
func = zeta^r*xi^p*eta^q;
F1 = int(func,xi,0,1-eta);
F1 = simplify(F1);
F2 = int(F1,eta,0,1);
F3 = factorial(p)*factorial(q)*factorial(r)/factorial(p+q+r+2);
DIFF2 = F2 - F3;
DIFF2 = vpa(DIFF2)




