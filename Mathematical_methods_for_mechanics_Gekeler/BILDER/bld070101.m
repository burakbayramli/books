function bld070101
% Balkenbiegung, Skizze 1
clc, clf
plot(-0.05,-1.2,'.w'), hold on
plot(2.2,0.5,'.w'), hold on

c = 0.1; d = 0.03;
X = [0,2.2]; Y = [0, 0];
arrow(X,Y,c,d,'k',2)
X = [0, 0]; Y = [0,-1.2];
arrow(X,Y,c,d,'k',2)
axis equal tight, axis manual

alfa = pi/6; beta = pi/2 - alfa;
X = [0,1.5*cos(alfa)]; Y = [0,-1.5*sin(alfa)];
XA = [0,1.8*cos(alfa)]; YA = [0,-1.8*sin(alfa)];
plot(XA,YA,'k','linewidth',2), hold on
X1 = [X(2) + 3*cos(beta), X(2) - 3*cos(beta)];
Y1 = [Y(2) + 3*sin(beta), Y(2) - 3*sin(beta)];
plot(X1,Y1,'k','linewidth',2), hold on
X2 = [X(2),X(2)]; Y2 = [0,Y(2)];
plot(X2,Y2,'k','linewidth',2), hold on
X3 = X(2) - Y(2)*tan(alfa);

%%%%%%%%%%%%%%%%%%%%%%%%%
rad = 0.018;
circle(X(2),0,rad,'w')
circle(X(2),Y(2),rad,'w')
circle(X3,0,rad,'w')

text(X(2)-0.15,-0.07,'x','Fontsize',30)
text(0.08,-0.5,'y = u(x)','Fontsize',30)
text(0.3,-0.07,'\alpha','Fontsize',30)
text(1.32,-0.45,'\beta','Fontsize',30)
text(1.4,-0.3,'Normale','Fontsize',30)
text(1.25,0.35,'y tan(\beta)','Fontsize',30)
X4 = [1.5,1.5]; Y4 = [0.2,0.05];
arrow(X4,Y4,c,d,'k',2)

grid off
axis off
