% BILD038
clf
clear
% -- Winkelbereich ------------------
a = 0.2;
LL = 5;
TT = pi/2 - a;
PHI = 1.35;
R = 15;
X3 = [-3+R*cos(TT),-3+R*cos(TT) + LL*cos(TT - PHI)];
Y3 = [-20+R*sin(TT),-20+R*sin(TT) + LL*sin(TT - PHI)];
plot(X3,Y3,'g','linewidth',2), hold on
X4 = [-3+R*cos(TT),-3+R*cos(TT) + LL*cos(TT + PHI)];
Y4 = [-20+R*sin(TT),-20+R*sin(TT) + LL*sin(TT + PHI)];
plot(X4,Y4,'r','linewidth',2), hold on
TTT = linspace(TT-PHI,TT+PHI,20);
X5 = [-3+ R*cos(TT), -3+R*cos(TT)+LL*cos(TTT)];
Y5 = [-20+R*sin(TT), - 20+R*sin(TT)+LL*sin(TTT)];
X6 = [X3,X5,fliplr(X4)];
Y6 = [Y3,Y5,fliplr(Y4)];
fill(X6,Y6,'y')
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5

R = [15:23]; a = 0.5;
TT = linspace(pi/2-2*a,pi/2+a,40);
for I = 1:length(R)
   X1 = - 3 + R(I)*cos(TT);
   Y1 = - 20 + R(I)*sin(TT);

   J = find(X1 < 10);
   X1 = X1(J); Y1 = Y1(J);
   J = find(X1 > - 10);
   X1 = X1(J); Y1 = Y1(J);
   plot(X1,Y1), hold on
end
R = 15; a = 0.2;
TT = pi/2 - a;
c = 0.5; d = 0.3;
% -- pos. und neg. Gradient von f ----------------
X2 = - 3  + [R*cos(TT), (R + 5.7)*cos(TT)];
Y2 = - 20 + [R*sin(TT), (R + 5.7)*sin(TT)];
arrow(X2,Y2,c,d,'k',2);
X2 = - 3  + [R*cos(TT), (R - 5.7)*cos(TT)];
Y2 = - 20 + [R*sin(TT), (R - 5.7)*sin(TT)];
arrow(X2,Y2,c,d,'k',2);
% -- Tangente -----------------------------
LL = 6;
X2 = - 3  + [R*cos(TT) - LL*sin(TT), R*cos(TT) + LL*sin(TT)];
Y2 = - 20 + [R*sin(TT) + LL*cos(TT), R*sin(TT) - LL*cos(TT)];
plot(X2,Y2,'k--','linewidth',2);
% -- Richtung d -------------------------------
X8 = - 3  + [R*cos(TT), R*cos(TT) + 7*cos(TT+1)];
Y8 = - 20 + [R*sin(TT), R*sin(TT) + 7*sin(TT+1)];
arrow_5(X8,Y8,c,d,2);
% -- Punkt x -----------------------------
X9 = - 3  + R*cos(TT);
Y9 = - 20 + R*sin(TT);
circle(X9,Y9,0.2,'w')
% -- Rahmen ------------------
plot(-10,-12.5,'w')
plot(10,3,'w')
RX = [-10,10,10,-10,-10];
RY = [-12.5,-12.5,3.3,3.3,-12.5];
plot(RX,RY,'k','linewidth',2)

text(1,0.8,'\nabla f(x)','fontsize',22)
text(-0.5,-10,'-\nabla f(x)','fontsize',22)
text(2,-7,'Tangente','fontsize',22)
text(-5,0.5,'d','fontsize',22)
axis equal
axis off
