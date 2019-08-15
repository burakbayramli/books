function bild01
% Figure for DEMO2-7

clf
load datenC7 YY OMGA MU
%MU
YY = [YY, YY(:,1)];
% -- X-Achse -------------
c  = 0.4; d  = 0.2; X1 = [-4, 4]; Y1 = [0,0];
%arrow(X1,Y1,c,d,'k',2), hold on
% -- Y-Achse --------------
c  = 0.4; d  = 0.2; X1 = [0,0]; Y1 = [-6,6];
%arrow(X1,Y1,c,d,'k',2), hold on
plot(YY(1,:),YY(2,:),'k','linewidth',2), hold on
plot(YY(3,:),YY(4,:),'r','linewidth',2), hold on
plot(YY(5,:),YY(6,:),'b','linewidth',2), hold on
axis equal% tight
grid on
