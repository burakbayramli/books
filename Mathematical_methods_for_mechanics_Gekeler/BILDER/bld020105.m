% BILD019B, Hermite-Grundpolynome fuer n = 3, H_1i
clf
X = linspace(-0.12,3.12,150);
L0 =  - (X - 1).*(X - 2).*(X - 3)/6;
L1 = X.*(X - 2).*(X - 3)/2;
L2 = - X.*(X - 1).*(X - 3)/2;
L3 = X.*(X - 1).*(X - 2)/6;
H10 = (X - 0).*L0.*L0;
H11 = (X - 1).*L1.*L1;
H12 = (X - 2).*L2.*L2;
H13 = (X - 3).*L3.*L3;

plot(X,H10,'linewidth',2), hold on
plot(X,H11,'linewidth',2), hold on
plot(X,H12,'linewidth',2), hold on
plot(X,H13,'linewidth',2), hold on
rr = 0.04;
circle(0,1,rr,'w')
circle(1,1,rr,'w')
circle(2,1,rr,'w')
circle(3,1,rr,'w')
circle(0,0,rr,'w')
circle(1,0,rr,'w')
circle(2,0,rr,'w')
circle(3,0,rr,'w')

plot(3.2,2,'w.'), hold on
plot(-0.2,-0.5,'w.'), hold on

text(0.2,0.3,'h_{10}','fontsize',24);
text(1.4,0.3,'h_{11}','fontsize',24);
text(1.4,-0.3,'h_{12}','fontsize',24);
text(2.7,-0.3,'h_{13}','fontsize',24);
axis equal tight
grid off
