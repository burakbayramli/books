function bld050101
% Pitchfork-Bifurkation
clf
% X-Achse -----------
% -- Rahmrn ------------
plot(-1,-2,'w.'), hold on
plot(3,2,'w.'), hold on
axis equal tight, axis manual
c = 0.15; d = 0.057;
X1 = [-1,0]; Y1 = [0,0];
plot(X1,Y1,'k','linewidth',2), hold on
X1 = [0,3]; Y1 = [0,0];
plot(X1,Y1,'k--','linewidth',2), hold on

X2 = [0,0]; Y2 = [-2,2];
arrow(X2,Y2,c,d,'k',2)
Y3 = linspace(-2,2,40);
X3 = 0.7*Y3.*Y3;
plot(X3,Y3,'k','linewidth',2)
text(0.18,1.8,'x','fontsize',22)
text(2.7,-0.2,'\mu','fontsize',22)

