function bld010101
% BLD001
clf
c = 0.15; d = 0.05;
X1 = [0,3]; Y1 = [0,1.5];
arrow(X1,Y1,c,d,'k',2)
X2 =  [2 ,1]; VEC = [-1,2];
X3 = X2 + 0.5*VEC;
X4 = [X2(1), X3(1)]; Y4 = [X2(2), X3(2)];
arrow(X4,Y4,c,d,'k',2)
X5 = [0, X3(1)]; Y5 = [0, X3(2)];
arrow(X5,Y5,c,d,'k',2)
X6 = [0,2]; Y6 = [0,1];
arrow(X6,Y6,c,d,'k',2)
% -- Viertelkreis ---------
A = pi/2+ 0.45;  B = pi+ 0.4; R = 0.2;
TT = linspace(A,B,40);
X6 = X2(1) + R*cos(TT);
Y6 = X2(2) + R*sin(TT);
plot(X6,Y6,'k','linewidth',2), hold on
X7 = X2(1)-0.1; Y7 = X2(2)+ 0.03;
circle(X7,Y7,0.01,'k')
circle(0,0,0.04,'w')
% -- Rahmen ------
RX = [-0.2,3.2,3.2,-0.2,-0.2];
RY = [-0.2,-0.2,2.2,2.2,-0.2];
plot(RX,RY,'k','linewidth',2)
text(0.85,1.5,'a','fontsize',22)
text(2.5,1,'b','fontsize',22)
text(1.1,0.3,'a_b','fontsize',22)
text(1.85,1.5,'a_n','fontsize',22)
grid on

axis equal
axis off
