% BILD019, Hermite-Grundpolynome fuer n = 3 , H_0i
clf
X = linspace(-0.12,3.12,150);
L0   =  - (X - 1).*(X - 2).*(X - 3)/6;
L0_1 = -((X-2).*(X-3) + (X-1).*(X-3) + (X-1).*(X-2))/6;
L1   = X.*(X - 2).*(X - 3)/2;
L1_1 = ((X-2).*(X-3) + X.*(X-3) + X.*(X-2))/2;
L2   = - X.*(X - 1).*(X - 3)/2;
L2_1 = - ((X-1).*(X-3) + X.*(X-3) + X.*(X-1))/2;
L3   = X.*(X - 1).*(X - 2)/6;
L3_1 = ((X-1).*(X-2) + X.*(X-2) + X.*(X-1))/6;
H00  = (1 - 2*L0_1.*(X - 0)).*L0.*L0;
H01  = (1 - 2*L1_1.*(X - 1)).*L1.*L1;
H02  = (1 - 2*L2_1.*(X - 2)).*L2.*L2;
H03  = (1 - 2*L3_1.*(X - 3)).*L3.*L3;

plot(X,H00,'linewidth',2), hold on
plot(X,H01,'linewidth',2), hold on
plot(X,H02,'linewidth',2), hold on
plot(X,H03,'linewidth',2), hold on
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

text(0.28,0.65,'h_{00}','fontsize',24);
text(0.7,1.4,'h_{01}','fontsize',24);
text(2,1.4,'h_{02}','fontsize',24);
text(2.4,0.65,'h_{03}','fontsize',24);
axis equal tight
grid off
