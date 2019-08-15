function bld060402
% Flaschenzug
clc, clear, clf
plot(-2.5,-3,'w.'), hold on
plot(4.5,6.5,'w.'), hold on
axis equal tight, axis manual
TT = linspace(0,2*pi,20);
X = cos(TT); Y = sin(TT);
fill(X,Y,'y'), hold on
plot(X,Y,'k','linewidth',2), hold on
X = 2 + cos(TT); Y = 4 + sin(TT);
fill(X,Y,'y'), hold on
plot(X,Y,'k','linewidth',2), hold on
X = [-1,-1]; Y = [6,0];
plot(X,Y,'k','linewidth',2), hold on
X = [1,1]; Y = [0,4];
plot(X,Y,'k','linewidth',2), hold on
X = [3,3]; Y = [4,-1.5];
plot(X,Y,'k','linewidth',2), hold on
X = [2.5,3.5,3.5,2.5,2.5]; Y = [-2.5,-2.5,-1.5,-1.5,-2.5];
fill(X,Y,'y'), hold on
plot(X,Y,'k','linewidth',2), hold on
X = [-0.5,0.5,0.5,-0.5,-0.5]; Y = [-2.5,-2.5,-1.5,-1.5,-2.5];
fill(X,Y,'y'), hold on
plot(X,Y,'k','linewidth',2), hold on
X = [0,0]; Y = [0,-1.5];
plot(X,Y,'k','linewidth',2), hold on
X = [2,2]; Y = [6,4];
plot(X,Y,'k','linewidth',2), hold on
X = [-2,4,4,-2,-2]; Y = [6,6,6.5,6.5,6];
fill(X,Y,'y'), hold on
X = [-2,4]; Y = [6,6];
plot(X,Y,'k','linewidth',2), hold on
X = [3,4.2]; Y = [-2,-2];
plot(X,Y,'k--','linewidth',2), hold on
X = [-2.2,1]; Y = [0,0];
plot(X,Y,'k--','linewidth',2), hold on
X = [1,4.2]; Y = [4,4];
plot(X,Y,'k--','linewidth',2), hold on
alfa = pi/3;
X = [2,2 + cos(alfa)]; Y = [4,4+sin(alfa)];
plot(X,Y,'k--','linewidth',2), hold on

X = [0,cos(alfa)]; Y = [0,sin(alfa)];
plot(X,Y,'k--','linewidth',2), hold on

c = 0.6; d = 0.2;
X = [4,4]; Y = [1.5,4];
arrow(X,Y,c,d,'k',1);
X = [4,4]; Y = [0.5,-2];
arrow(X,Y,c,d,'k',1);
X = [-2,-2]; Y = [3.5,6];
arrow(X,Y,c,d,'k',1);
X = [-2,-2]; Y = [2.5,0];
arrow(X,Y,c,d,'k',1);

rr = 0.1;
circle(-1,6,rr,'w')
circle(-1,6,rr,'w')
circle(2,6,rr,'w')
circle(0,0,rr,'w')
circle(2,4,rr,'w')
circle(0,-2,rr,'w')
circle(3,-2,rr,'w')
text(-1.4,-2.1,'m_1','fontsize',18)
text(1.6,-2.1,'m_2','fontsize',18)
text(3.7,1,'s_1','fontsize',18)
text(-2.3,3,'s_2','fontsize',18)
text(2.3,4.3,'\phi_1','fontsize',18)
text(0.3,0.3,'\phi_2','fontsize',18)
text(1.5,3.7,'r','fontsize',18)
text(-0.5,-0.3,'r','fontsize',18)

grid off
axis off
