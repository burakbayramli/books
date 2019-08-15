% BILD045
clf
X3 = linspace(-sqrt(2),sqrt(2),40);
Y3 = 1 - X3.*X3/2;
Y4 = X3.*X3/2 - 1;
X5 = [X3,fliplr(X3)];
Y5 = [Y3,fliplr(Y4)];
fill(X5,Y5,'y'), hold on

X1 = linspace(-1.7,1.7,40);
Y1 = 1 - X1.*X1/2;
plot(X1,Y1,'k','linewidth',2), hold on
Y2 = X1.*X1/2 - 1;
plot(X1,Y2,'k','linewidth',2), hold on

X3 = [0,4];
Y3 = [1,1];
c = 0.17; d = 0.09;
arrow(X3,Y3,c,d,'k',2)
plot(X3,Y3), hold on
X4 = [4,4];
Y4 = [1.2,-0.7];
plot(X4,Y4,'k--','linewidth',2), hold on
X5 = [2,2];
Y5 = [1.2,-0.7];
plot(X5,Y5,'k--','linewidth',2), hold on
X6 = [1,1];
Y6 = [1.2,-0.7];
plot(X6,Y6,'k--','linewidth',2), hold on

%circle(4,1,0.04,'w')
circle(2,1,0.04,'w')
circle(1,1,0.04,'w')
circle(0,1,0.04,'w')
circle(1,1/2,0.06,'w')

X3 = [1.5,1.1];
Y3 = [-0.6,0.3];
c = 0.09; d = 0.06;
arrow(X3,Y3,c,d,'k',1)
% -- Rahmen --------------------
RX = [-2,4.5,4.5,-2,-2];
RY = [-1.5,-1.5,1.5,1.5,-1.5];
plot(RX,RY,'k','linewidth',2)

text(-0.2,0,'S','fontsize',30)
text(3.5,1.25,'d','fontsize',18)
text(3.35,0.75,'\sigma=1','fontsize',18)
text(2.1,0.75,'\sigma=1/2','fontsize',18)
text(1,0.75,'\sigma=1/4','fontsize',18)
text(-1,1.2,'alter Punkt','fontsize',18)
text(1.5,-0.75,'neuer Punkt','fontsize',18)

axis equal tight
grid on
axis off
