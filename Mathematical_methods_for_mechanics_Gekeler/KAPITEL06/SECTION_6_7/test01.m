function test01
% top with Euler angles
% Inversion of the matrix
clc
syms T1 T1 T3 thet
A = [T1*sin(thet)*sin(thet) + T3*cos(thet)*cos(thet), 0, T3*cos(thet);
     0, T1, 0; T3*cos(thet), 0, T3];
DETA = det(A); DETA = simplify(DETA)
B = inv(A); B = DETA*B; B = simplify(B)
AINV = [T3, 0, -T3*cos(thet);
        0, T3*sin(thet)*sin(thet), 0;
        -T3*cos(thet), 0, T1*sin(thet)*sin(thet)+T3*cos(thet)*cos(thet)];
AINV = AINV/(T1*T3*sin(thet)*sin(thet));
C = A*AINV; C = simplify(C)
