function rt_aux
clc
% Maple-File fuer RT-Element (Rannacher-Turek)
syms x1 x2 x3 x4 y1 y2 y3 y4 x y
s1 = (x1+x2+x3+x4)/4;
s2 = (y1+y2+y3+y4)/4;
ACHSE1 = [(x2+x3)/2-s1,(y2+y3)/2- s2];
ACHSE2 = [(x3+x4)/2-s1,(y3+y4)/2- s2];
ACHSE1 = simplify(ACHSE1); ACHSE2 = simplify(ACHSE2);

SP = ACHSE1*ACHSE2.';
SP = 16*SP;
SP = simplify(SP);
% Probe auf Orthogonalitaet der lokalen Achsen
S1 = [(x1+x2)/2,(y1+y2)/2]; S2 = [(x2+x3)/2,(y2+y3)/2];
S3 = [(x3+x4)/2,(y3+y4)/2]; S4 = [(x4+x1)/2,(y4+y1)/2];
ACHSE1 = S3 - S1; ACHSE2 = S2 - S4;
SP1 = ACHSE1*ACHSE2.';
SP1 = 16*SP1; SP1 = simplify(SP1); SP1 = simplify(SP1);
SP1 = SP1/4;
DIFF = SP - SP1;
DIFF = simplify(DIFF);
% Ergebnis: lokale Achsen sind nicht orthogoanl
%Design-Matrix
A = [1, 0, -1, -1; 1, 1, 0, 1; 1, 0, 1, -1; 1, -1, 0, 1]
B = inv(A);
B = 4*B;
FF = [1,x1,y1, x1^2 - y1^2];
FF = FF*B;
FF = simplify(FF);
b1 = (x2+x3-x1-x4)/4; %/4
b2 = (y2+y3-y1-y4)/4; %/4
c1 = (x3+x4-x1-x2)/4; %/4
c2 = (y3+y4-y1-y2)/4; %/4
xi = c2*(x - s1) -c1*(y-s2);
xi = simplify(xi);
 
eta = -b2*(x - s1) + b1*(y-s2);
eta = simplify(eta);
det = b1*c2-b2*c1;
FF1 = (det*det - 2*eta*det - xi^2 + eta^2)*16;
FF1 = simplify(FF1)

FF2 = (1 + 2*xi  + xi^2 - eta^2)/4;
FF3 = (1 + 2*eta - xi^2 + eta^2)/4;
FF4 = (1 - 2*xi  + xi^2 - eta^2)/4; 


