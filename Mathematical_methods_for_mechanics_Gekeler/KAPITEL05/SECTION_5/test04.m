function test04
% Check permutation of U and C in Hopf bifurcation
clc
syms a t c11 c12 c21 c22
c1 = [c11;c12]; c2 = [c21;c22];
C = [cos(a), -sin(a); sin(a), cos(a)];
U1 = [c1*cos(t)-c2*sin(t), c1*sin(t)+c2*cos(t)];
U2 = [c1*cos(t+a)-c2*sin(t+a), c1*sin(t+a)+c2*cos(t+a)];

A = U1*C; 
DIFF = A - U2;
DIFF = simplify(DIFF) 

flag = 0;
if flag == 1
cc = rand(1,4)*10;
c1 = [cc(1);cc(2)]; c2 = [cc(3);cc(4)];
t = pi/6; a = pi/8;

DIFF1 = ...
[ -sin(a)*(cos(t)*c12-sin(t)*c22+sin(t)*c11+cos(t)*c21), ...
 sin(a)*(cos(t)*c11-sin(t)*c21-sin(t)*c12-cos(t)*c22);
 sin(a)*(cos(t)*c11-sin(t)*c21-sin(t)*c12-cos(t)*c22), ...
sin(a)*(cos(t)*c12-sin(t)*c22+sin(t)*c11+cos(t)*c21)];
DNORM1 = norm(DIFF1)


DIFF2 = ...
[ sin(a)*(-cos(t)*c12+sin(t)*c22+sin(t)*c11+cos(t)*c21), ...
-sin(a)*(cos(t)*c11-sin(t)*c21+sin(t)*c12+cos(t)*c22);
sin(a)*(cos(t)*c11-sin(t)*c21+sin(t)*c12+cos(t)*c22), ... 
sin(a)*(-cos(t)*c12+sin(t)*c22+sin(t)*c11+cos(t)*c21)];
DNORM2 = norm(DIFF2)
end 


