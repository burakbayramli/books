% Beispiel fuer Bezier-Polynom vom Grad n = 3
% das Polynom wird an der Stelle X berechnet
clf
SKAL = 6;
X = 0.4;
B = [1 4   3 0]; % Bezier-Punkte
% --------------------------------
X1 = SKAL*[0 1/3 2/3 1]; Y1 = [0 0 0 0];
plot(X1,Y1,'k','linewidth',2), hold on
X2 = [0 1/3 2/3] + X/3; Y2 = [0 0 0];
% -------------------------------
X3 = SKAL*[0 1/3 2/3 1]; Y3 = B;
plot(X3,Y3,'k','linewidth',2), hold on
% -------------------------------
X4 = SKAL*([0 1/3 2/3] + X/3); Y4 = [2.2 3.6 1.8];
plot(X4,Y4,'k--','linewidth',2), hold on
% ----------------------------
X5 = SKAL*([0 1/3] + 2*X/3); Y5 = [2.76 2.88];
plot(X5,Y5,'k:','linewidth',2), hold on
% ----------------------------
X6 = SKAL*0.4; Y6 = 2.81;
% ----------------------------
X = linspace(0,1,100);
P = decastel(B,X);
rr = 0.07;
plot(SKAL*X,P,'k','linewidth',2)
for I = 1:length(X1)
   circle(X1(I),0,rr,'w')
end
for I = 1:length(X3)
   circle(X3(I),Y3(I),rr,'w')
end
for I = 1:length(X4)
   circle(X4(I),Y4(I),rr,'w')
   circle(X4(I),0,rr,'y')
end
for I = 1:length(X5)
   circle(X5(I),Y5(I),rr,'w')
   circle(X5(I),0,rr,'k')
end
circle(X6,Y6,rr,'w')
circle(X6,0,rr,'k')
grid on
axis equal tight
%xlabel('X');
%ylabel('Y');
text(0.2,0.9,'b_0','fontsize',22);
text(1.95,3.6,'b_1','fontsize',22);
text(4.2,2.9,'b_2','fontsize',22);
text(5.3,0.25,'b_3','fontsize',22);
text(0.3,2.1,'b_{01}','fontsize',22);
text(3,3.7,'b_{12}','fontsize',22);
text(5,1.75,'b_{23}','fontsize',22);
grid off
