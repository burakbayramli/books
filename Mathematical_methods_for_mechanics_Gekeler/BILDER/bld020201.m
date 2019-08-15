function bld020201
% Legendre-Polynome
clf,clear
X = linspace(-1,1,60);
P2  =  (3*X.*X - 1)/2;
plot(X,P2,'k','linewidth',2),hold on
P3 = (5*X.*X.*X - 3*X)/2;
plot(X,P3,'--','linewidth',2),hold on
P4 = (35*X.^4 - 30*X.*X + 3)/8;
plot(X,P4,':','linewidth',2),hold on
P5 = (63*X.^5 - 70*X.^3 + 15*X)/8;
plot(X,P5,'-.','linewidth',2),hold on
P6 = (231*X.^6 - 315*X.^4 + 105*X.*X - 5)/16;
plot(X,P6,'linewidth',2)
grid on
text(-0.9,0.86,'p_2','fontsize',22);
text(-0.35,0.47,'p_3','fontsize',22);
text(0.58,0.3,'p_4','fontsize',22);
text(0.3,0.42,'p_5','fontsize',22);
text(0.02,0.47,'p_6','fontsize',2);
grid off
