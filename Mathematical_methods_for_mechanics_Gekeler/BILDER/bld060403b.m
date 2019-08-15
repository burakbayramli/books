function bld110503
% Flaschenzug
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
plot(X1(1,:),X1(2,:),'linewidth',2), hold on
fill(X1(1,:),X1(2,:),'y'), hold on
A = [-2.2,2.2;0,0];
X2 = DD*A;
plot(X2(1,:),X2(2,:),'linewidth',2), hold on
XA = [-1.5,-0.5]; YA = [1,1];
plot(XA,YA,'k--'), hold on
XA = [-1,-1]; YA = [0.5,1.5];
plot(XA,YA,'k--'), hold on

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
a = 1.5; b = 0.8;
SS1 = linspace(1,0.5,20); SS2 = linspace(0.5,1,20);
SS = [SS1,SS2];
X = a*cos(TT); Y = b*SS.*sin(TT);
phi = 5*pi/12;
DD = [cos(phi), - sin(phi);sin(phi), cos(phi)];
X3 = DD*[X;Y];
%plot(X3(1,:),X3(2,:),'linewidth',2), hold on

X4 = X3 + [0.8*ones(1,40);-2.2*ones(1,40)];
plot(X4(1,:),X4(2,:),'linewidth',2), hold on
fill(X4(1,:),X4(2,:),'y'), hold on
A = [-1.7,1.7;0,0];
X3 = DD*A;
X4 = X3 + [0.8*ones(1,2);-2.2*ones(1,2)];
plot(X4(1,:),X4(2,:),'r','linewidth',2), hold on
plot(X1(1,:),X1(2,:),'linewidth',2), hold on
plot(X2(1,:),X2(2,:),'linewidth',2), hold on

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
rr = 0.05; ss = 0.08;
circle(-1,1,rr,'w')
circle(1.1,-1.1,rr,'w')
P = [1.1,-1.1];
circle(0.3,-0.3,ss,'r')
Q = [0.8,-2.2];
S = Q-P;
T = P + 1.3*S;
circle(T(1),T(2),ss,'w')
grid off
axis off
