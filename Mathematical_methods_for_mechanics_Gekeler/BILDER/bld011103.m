% BILD012, Skizze zu  Projektor: Stetigkeit + Kontraktion
clf
CC = 30;
PHI1 = 2*pi*(90-CC)/360;
PHI2 = 2*pi*(90+CC)/360;
T = linspace(PHI1,PHI2,30);
X5 = 8*cos(T);
Y5 = 8*sin(T);
plot(X5,Y5,'linewidth',2), hold on
CC = 20;
PHI1 = 2*pi*(90-CC)/360;
PHI2 = 2*pi*(90+CC)/360;
PV = [8*cos(PHI1),8*sin(PHI1)];
PW = [8*cos(PHI2),8*sin(PHI2)];
V = [13*cos(PHI1),13*sin(PHI1)];
W = [13*cos(PHI2),13*sin(PHI2)];

S1X = [V(1),PV(1)];
S1Y = [V(2),PV(2)];
plot(S1X,S1Y,'k','linewidth',2)
S2X = [W(1),PW(1)];
S2Y = [W(2),PW(2)];
plot(S2X,S2Y,'k','linewidth',2)

circle(PV(1),PV(2),0.15,'w')
circle(PW(1),PW(2),0.15,'w')
circle(V(1),V(2),0.15,'w')
circle(W(1),W(2),0.15,'w')
% -- Rahmen -------------
RX = [-5,5,5,-5,-5];
RY = [6.5,6.5,13,13,6.5];
plot(RX,RY,'k','linewidth',2)
axis equal
grid on
text(3.8,12,'v','fontsize',26)
text(-4.1,12,'w','fontsize',26)
text(1.9,8.3,'Pv','fontsize',26)
text(-2.6,8.3,'Pw','fontsize',26)
axis off
%axis off
