% Bild 021A, Einheitsdreieck
clf,
c = 0.07; d = 0.03;
% -- X-Achse ------------
X = [-0.2,1.2]; Y = [0,0];
arrow(X,Y,c,d,'k',1), hold on
% -- Y- Achse ---------
X = [0,0]; Y = [-0.2,1.2];
arrow(X,Y,c,d,'k',1), hold on
X1 = [0;1]; Y1 = [0;0];
plot(X1,Y1,'k','linewidth',2), hold on
X1 = [0;0]; Y1 = [0;1];
plot(X1,Y1,'k','linewidth',2), hold on
X1 = [1;0]; Y1 = [0;1];
plot(X1,Y1,'k','linewidth',2), hold on

U1 = 0; V1 = 0; U2 = 1; V2 = 0; U3 = 0; V3 = 1;

X1 = 1/2;   Y1 = 0;
X2 = 1/2;   Y2 = 1/2;
X3 = 0;     Y3 = 1/2;
X4 = [0,1/2]; Y4 = [0,1/2];
plot(X4,Y4,'--','linewidth',2), hold on
X5 = [1, 0]; Y5 = [0,1/2];
plot(X5,Y5,'--','linewidth',2), hold on
X6 = [0, 1/2]; Y6 = [1, 0];
plot(X6,Y6,'--','linewidth',2), hold on

circle(X1,Y1,0.03,'w');
circle(X2,Y2,0.03,'w');
circle(X3,Y3,0.03,'w')
text(0.46,-0.1,'1','FontSize',26)
text(0.54,0.58,'2','FontSize',26)
text(-0.12,0.49,'3','FontSize',26)
text(1.1,-0.08,'\xi','fontsize',26)
text(0.06,1.1,'\eta','fontsize',26)

grid off
axis equal tight
%axis off

