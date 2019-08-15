function bild02
load daten M_A X Y PP
clf
%axis([-4 4 -4 4])
axis([-8 8 -8 8])
grid on, axis equal, axis manual, hold on
AUX1 = [X, X(:,1)];
AUX2 = [Y, Y(:,1)];
plot(AUX1(1,:),AUX1(2,:),'r'), hold on
plot(AUX2(1,:),AUX2(2,:),'b','linewidth',2), hold on
plot(AUX1(1,1),AUX1(2,1),'or'), hold on

plot(AUX1(1,:),AUX1(2,:),'.r'), hold on
plot(AUX2(1,:),AUX2(2,:),'.b'), hold on

plot(PP(1,:),PP(2,:),'og'), hold on
circle(M_A(1),M_A(2),0.05,'k')
