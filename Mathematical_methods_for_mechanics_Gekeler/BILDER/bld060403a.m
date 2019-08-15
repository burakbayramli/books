function bld110503
% Doppelpendel
clc, clear, clf
plot(-2,-4,'w.'), hold on
plot(2,1.5,'w.'), hold on
axis equal tight, axis manual
TT = linspace(-pi,pi,40);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
a = 2; b = 1;
SS1 = linspace(1,0.5,20); SS2 = linspace(0.5,1,20);
SS = [SS1,SS2];
X = a*cos(TT); Y = SS.*sin(TT);
phi = 3*pi/4;
DD = [cos(phi), - sin(phi);sin(phi), cos(phi)];
X1 = DD*[X;Y];
%plot(X1(1,:),X1(2,:),'linewidth',2), hold on
A = [-2.2,2.2;0,0];
X2 = DD*A;
plot(X2(1,:),X2(2,:),'linewidth',2), hold on
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
a = 1.5; b = 0.8;
SS1 = linspace(1,0.5,20); SS2 = linspace(0.5,1,20);
SS = [SS1,SS2];
X = a*cos(TT); Y = b*SS.*sin(TT);
phi = 5*pi/12;
DD = [cos(phi), - sin(phi);sin(phi), cos(phi)];
X1 = DD*[X;Y];
%plot(X1(1,:),X1(2,:),'linewidth',2), hold on

X2 = X1 + [0.8*ones(1,40);-2.2*ones(1,40)];
%plot(X2(1,:),X2(2,:),'linewidth',2), hold on
A = [-1.7,1.7;0,0];
X3 = DD*A;
X4 = X3 + [0.8*ones(1,2);-2.2*ones(1,2)];
plot(X4(1,:),X4(2,:),'r','linewidth',2), hold on
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
A = [-1,1]; B = [-1.3,1]; PHI = 3*pi/2;
[X,Y] = segm(A,B,PHI,0);
plot(X,Y,'linewidth',2), hold on
A = [1.1,-1.1]; B = [0.8,-1.1];
[X,Y] = segm(A,B,PHI,0);
plot(X,Y,'linewidth',2), hold on
A = [-1,1]; B = [-1,-0.3]; PHI = 0.8;
[X,Y] = segm(A,B,PHI,0);
plot(X,Y,'linewidth',2), hold on
A = [1.1,-1.1]; B = [1.1,-2.2]; PHI = -0.28;
[X,Y] = segm(A,B,PHI,0);
plot(X,Y,'linewidth',2), hold on
X = [-1.15,-1,-1,-1.15]; Y = [1.3,1.27,1.37,1.3];
fill(X,Y,'k')
X = [0.95,1.1,1.1,0.95]; Y = [-0.8,-0.85,-0.75,-0.8];
fill(X,Y,'k')
X = [-1,-1];Y = [1.5,-0.5];
plot(X,Y,'k--'), hold on
X = [1.1,1.1];Y = [-1.1,-2.5];
plot(X,Y,'k--'), hold on

rr = 0.07; ss = 0.12;
circle(-1,1,rr,'w')
circle(1.1,-1.1,rr,'w')
P = [1.1,-1.1];
circle(0.3,-0.3,ss,'y')
Q = [0.8,-2.2];
S = Q-P;
T = P + 1.3*S;
circle(T(1),T(2),ss,'y')

text(-0.25,0.5,'l_1','fontsize',22)
text(0.4,-0.1,' l','fontsize',22)
text(-0.3,-0.5,'S_1','fontsize',22)
text(-0.8,-3,'S_2(x_2,y_2)','fontsize',22)
text(0.45,-1.8,'l_2','fontsize',22)
text(-0.8,0.2,'\phi_1','fontsize',22)
text(0.9,-2,'\phi_2','fontsize',22)
text(-0.3,-1.2,'(x_1,y_1)','fontsize',22)
grid off
axis off
