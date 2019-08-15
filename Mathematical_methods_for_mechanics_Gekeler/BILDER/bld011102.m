function bld011102
% Skizze zum  Projektionssatz
clf
a = 0.07; r = 1/2;
A = [0;  0]; B = [-4; 0]; C = [-2; 0];
PHI = pi/4;
D = [-2+cos(PHI); sin(PHI)];
E = C + r*(D-C);
PSI = 2*pi*130/360;
[X5,Y5] = segm(C,E,PSI,0);
plot(X5,Y5,'linewidth',2), hold on

X6 = [-2.7, -2.2]; Y6 = [0.7,0.2];
arrow_4(X6,Y6,0.2,0.1,'k',2)
X1 = [-2;-4]; Y1 = [0; 0];
plot(X1,Y1,'linewidth',2), hold on
X2 =[-2; -2 + cos(PHI)];
Y2 = [0; sin(PHI)];
plot(X2,Y2,'linewidth',2), hold on

CC = 50;
PHI1 = 2*pi*(180-CC)/360;
PHI2 = 2*pi*(180+CC)/360;
T = linspace(PHI1,PHI2,30);
X5 = 2*cos(T);
Y5 = 2*sin(T);
plot(X5,Y5,'linewidth',2), hold on

circle(B(1),B(2),a,'w')
circle(C(1),C(2),a,'w')
circle(D(1),D(2),a,'w')

axis equal
grid on
text(-2.3,-0.25,'u','fontsize',26)
text(-1.3,1,'v','fontsize',26)
text(-4.2,-0.25,'w','fontsize',26)
text(-3.5,1,'\phi \geq \pi/2','fontsize',26)
%title('Zu Lemma 33(a)','fontsize',26)
% -- Rahmen
RX = [-4.4,-1,-1,-4.4,-4.4];
RY = [-1.6,-1.6,1.6,1.6,-1.6];
plot(RX,RY,'k','linewidth',2)
axis equal tight
axis off
