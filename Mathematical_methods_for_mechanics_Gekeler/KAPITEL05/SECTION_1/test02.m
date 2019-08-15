function test03

cc = rand(1,4)*10;
c11 = cc(1); c12 = cc(2); c21 = cc(3); c22 = cc(4);
t = pi/6; a = pi/8;

c1 = [c11;c12]; c2 = [c21;c22];
C = [cos(a), -sin(a); sin(a), cos(a)];
U1 = [c1*cos(t)-c2*sin(t), c1*sin(t)+c2*cos(t)];
U2 = [c1*cos(t+a)-c2*sin(t+a), c1*sin(t+a)+c2*cos(t+a)];

DIFF = U1*C.' - U2 


