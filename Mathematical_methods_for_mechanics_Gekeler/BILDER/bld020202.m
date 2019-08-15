% BILD020202.M, Tschebyscheff-Polynome fuer n = 3
clf
XX = linspace(-1,1,100);
YY1 = cos(2*acos(XX));
plot(XX,YY1,'k','linewidth',2), hold on
YY1 = cos(3*acos(XX));
plot(XX,YY1,'k','linewidth',2), hold on
YY1 = cos(4*acos(XX));
plot(XX,YY1,'k','linewidth',2), hold on
YY1 = cos(5*acos(XX));
plot(XX,YY1,'k','linewidth',2), hold on
grid on
text(-0.05,-0.85,'p_2','fontsize',22);
text(0.18,-0.3,'p_3','fontsize',22);
text(-0.32,0,'p_4','fontsize',22);
text(0.5,0.7,'p_5','fontsize',22);
axis equal tight
grid off
