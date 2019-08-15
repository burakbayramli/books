function bld060401
% Beispiel zum D'Alembert-Lagrange-Prinzip
% nach Szabo
clc, clear, clf
plot(-3.3,-4.7,'w.'), hold on
plot(4.3,4.3,'w.'), hold on
X = [-3,4,4,-3,-3];
Y = [4,4,4.3,4.3,4];
fill(X,Y,'y'), hold on
TT = linspace(0,2*pi,40);
R1 = 1; R2 = 2;
X1 = [R1*cos(TT);R1*sin(TT)];
X2 = [R2*cos(TT);R2*sin(TT)];
plot(X1(1,:),X1(2,:),'linewidth',2), hold on
plot(X2(1,:),X2(2,:),'linewidth',2), hold on
fill(X1(1,:),X1(2,:),'y'), hold on
X = [-3,4]; Y = [4,4];
plot(X,Y,'linewidth',2), hold on
X = [1,1]; Y = [0,4];
plot(X,Y,'linewidth',2), hold on
X = [2,2]; Y = [0,-3.5];
plot(X,Y,'linewidth',2), hold on

X = [0,2.7]; Y = [0,0];
plot(X,Y,'k--','linewidth',2,'erasemode','none'), hold on

X = [1.5,2.5,2.5,1.5,1.5];
Y = [-4.5,-4.5,-3.5,-3.5,-4.5];
plot(X,Y,'linewidth',2), hold on
fill(X,Y,'y'), hold on
X = [2,3.5]; Y = [-4,-4];
plot(X,Y,'k--','linewidth',2,'erasemode','none'), hold on

c = 0.3; d = 0.1;
X = [2.5,2.5]; Y = [4,2.4];
plot(X,Y,'k'), hold on
X = [2.5,2.5]; Y = [1.6,0];
arrow(X,Y,c,d,'k',1)
alfa = 5*pi/4;
X = [0,cos(alfa)]; Y = [0,sin(alfa)];
plot(X,Y,'k:','linewidth',2,'erasemode','none'), hold on

alfa = 3*pi/4;
X = [0,R2*cos(alfa)]; Y = [0,R2*sin(alfa)];
plot(X,Y,'k:','linewidth',2,'erasemode','none'), hold on


X = [3,3]; Y = [4,-1.6];
plot(X,Y,'k'), hold on
X = [3,3]; Y = [-2.4,-4];
arrow(X,Y,c,d,'k',1)
rr = 0.09;
circle(1,4,rr,'k')
circle(2.5,4,rr,'w')
circle(3,4,rr,'w')
circle(0,0,rr,'w')
circle(2,-4,rr,'w')
text(0.6,-4,'m_1','fontsize',18)
text(-2.2,-1.6,'m_2','fontsize',18)
text(2.4,2,'s','fontsize',18)
text(2.9,-2,'q','fontsize',18)
text(-0.2,-0.5,'r','fontsize',18)
text(-1,1.2,'R','fontsize',18)

text(1,-2.5,'(B)','fontsize',18)
text(0,2.8,'(A)','fontsize',18)


axis equal tight
axis off
grid off
