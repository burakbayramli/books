function triaux
syms x y
A = [1  x  y; x  x*x x*y; y x*y y*y];
N1 = (1-x)*(1-y);N2 = x*(1-y);N4 = x*y;N3 = (1-x)*y;
A1 = N1*A;
B1 = int(A1,y,0,1);
C1 = int(B1,0,1);
A2 = N2*A;
B2 = int(A2,y,0,1);
C2 = int(B2,0,1);
A3 = N3*A;
B3 = int(A3,y,0,1);
C3 = int(B3,0,1);
A4 = N4*A;
B4 = int(A4,y,0,1);
C4 = int(B4,0,1);
C1 = 360*C1
C2 = 360*C2
C3 = 360*C3
C4 = 360*C4
