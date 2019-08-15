% bld040102, Legendre-Transformation
clf
% -- X-Achse ----------------------
c = 0.1; d = 0.03;
X1 = [-0.5,1.5]; Y1 = [0, 0];
arrow(X1,Y1,c,d,'k',2)
% -- Y-Achse -------------------
X1 = [0, 0]; Y1 = [-0.5, 1.5];
arrow(X1,Y1,c,d,'k',2)
X2 = linspace(0,1.2,20);
Y2 = X2.*X2;
plot(X2,Y2,'k','linewidth',2); hold on
X3 = [-0.3, 1.5]; Y3 = [-0.3,1.5];
plot(X3,Y3,'k','linewidth',2), hold on
X3 = [1/4, 3/4]; Y3 = [1/4,3/4];
Y3 = Y3 - 1/4;
plot(X3,Y3,'k--','linewidth',2), hold on

X4 = [1/2, 1/2]; Y4 = [-0.1,1/2];
plot(X4,Y4,'k:','linewidth',2), hold on

RR = 0.025;
circle(1/2,1/4,RR,'w')
circle(1/2,1/2,RR,'w')
circle(1/2,0,RR,'w')

% --Rahmen --------------------------
X5 = [-0.5,1.5,1.5,-0.5,-0.5];
Y5 = [-0.5 ,-0.5,1.5, 1.5, -0.5];
plot(X5,Y5,'k','linewidth',2)

text(1.35,-0.1,'x','fontsize',22)
text(-0.13,1.4,'y','fontsize',22)
text(0.3,0.7,'y = px','fontsize',22)
text(0.9,0.7,'y = f(x)','fontsize',22)
text(0.4,-0.15,'x(p)','fontsize',22)
text(0.9,0.2,'g(p)','fontsize',22)
X = [0.85,0.55]; Y = [0.2,0.4];
arrow(X,Y,c,d,'k',2)


grid on
axis equal tight
axis off
grid off
