% Formfunktionen fuer lineare Dreieckselemente
clc
syms x1 x2 x3 y1 y2 y3 x y
x21 = x2 - x1; x31 = x3 - x1; x32 = x3 - x2;
y21 = y2 - y1; y31 = y3 - y1; y32 = y3 - y2;
J = x21*y31 - x31*y21;
A1 = J - y32*(x-x1) + x32*(y-y1);
A2 = x2*y3 - x3*y2 - y32*x + x32*y;
DIFFA = A1 - A2;
DIFFA = simplify(DIFFA)
B1 = J + y31*(x-x2) - x31*(y-y2);
B2 = x3*y1 - x1*y3 + y31*x - x31*y;
DIFFB = B1 - B2;
DIFFB = simplify(DIFFB)
C1 = J - y21*(x-x3) + x21*(y-y3);
C2 = x1*y2 - x2*y1 - y21*x + x21*y;
DIFFC = C1 - C2;
DIFFC = simplify(DIFFC)

