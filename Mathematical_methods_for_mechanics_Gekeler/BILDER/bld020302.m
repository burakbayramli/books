% Bild 021B, Einheitsdreieck
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

X1 = 1/3;                 Y1 = 1/3;
X2 = (6 + sqrt(15))/21;   Y2 = X2;
X3 = (9 - 2*sqrt(15))/21; Y3 = Y2;
X4 = X2 ;                 Y4 = X3;
X5 = (6 - sqrt(15))/21;   Y5 = X5;
X6 = (9+ 2*sqrt(15))/21;  Y6 = X5;
X7 = X5; Y7 = X6;
s = 1.1;
X8 = [0; s*X4]; Y8 = [1; 1 + s*(Y4-1)];
plot(X8,Y8,'--','linewidth',2), hold on
X9 = [0; s*X2]; Y9 = [0; s*Y2];
plot(X9,Y9,'--','linewidth',2), hold on
X10 = [1; 1 + s*(X3-1)]; Y10 = [0; s*Y2];
plot(X10,Y10,'--','linewidth',2), hold on
rr = 0.03;
circle(X1,Y1,rr,'w');
circle(X2,Y2,rr,'w');
circle(X3,Y3,rr,'w')
circle(X4,Y4,rr,'w')
circle(X5,Y5,rr,'w')
circle(X6,Y6,rr,'w')
circle(X7,Y7,rr,'w')
text(0.3,0.23,'1','FontSize',26)
text(0.47,0.37,'2','FontSize',26)
text(0.04,0.37,'3','FontSize',26)
text(0.36,0.06,'4','Fontsize',26)
text(0.14,0.05,'5','Fontsize',26)
text(0.69,0.05,'6','Fontsize',26)
text(0.06,0.69,'7','Fontsize',26)
text(1.1,-0.06,'\xi','fontsize',26)
text(0.06,1.1,'\eta','fontsize',26)
grid off
axis equal tight
%axis off

