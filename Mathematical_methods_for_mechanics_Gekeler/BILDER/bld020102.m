% BILD018, Lagrange-Grundpolynome fuer n = 3

clf
X = linspace(-0.15,3.1,40);
L0 =  - (X - 1).*(X - 2).*(X - 3)/6;
plot(X,L0,'linewidth',2),hold on
L1 = X.*(X - 2).*(X - 3)/2;
plot(X,L1,'linewidth',2),hold on
L2 = - X.*(X - 1).*(X - 3)/2;
plot(X,L2,'linewidth',2),hold on
L3 = X.*(X - 1).*(X - 2)/6;
plot(X,L3,'linewidth',2),hold on
plot(3.2,1.5,'w.'), hold on
plot(-0.2,-0.5,'w.'), hold on

grid on
%title(' Lagrange-Grundpolynome, n = 3','fontsize',18)
%xlabel('X');
%ylabel('Y');
rr = 0.04;
circle(0,1,rr,'w')
circle(1,1,rr,'w')
circle(2,1,rr,'w')
circle(3,1,rr,'w')
circle(0,0,rr,'w')
circle(1,0,rr,'w')
circle(2,0,rr,'w')
circle(3,0,rr,'w')

text(0.4,0.48,'q_0','fontsize',22);
text(0.9,0.85,'q_1','fontsize',22);
text(2,0.85,'q_2','fontsize',22);
text(2.3,0.48,'q_3','fontsize',22);
axis equal tight
grid off
