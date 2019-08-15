function bld060801
% Doppelpendel, Kurve der beiden Massepunkte
%disp('Zuerst DEMO1.M, Beispiel 3  Aufrufen! ')
clf
load daten3 tt uout vout wout
XX = uout;
plot(uout(1,:),uout(2,:),'k','linewidth',2), hold on
plot(uout(3,:),uout(4,:),'r','linewidth',2), hold on
NN = 1;
XX1 = [uout(1,NN),uout(3,NN)];
YY1 = [uout(2,NN),uout(4,NN)];
rr = 0.04;
plot(XX1,YY1,'k--','linewidth',2), hold on
circle(XX1(1),YY1(1),rr,'w')
circle(XX1(2),YY1(2),rr,'w')

MM = length(tt);
MM1 = MM/4; MM1 = fix(MM1);
NN = MM1;
XX1 = [uout(1,NN),uout(3,NN)];
YY1 = [uout(2,NN),uout(4,NN)];
rr = 0.04;
plot(XX1,YY1,'b--','linewidth',2), hold on
circle(XX1(1),YY1(1),rr,'w')
circle(XX1(2),YY1(2),rr,'w')
MM2 = MM/2; MM2 = fix(MM2);
NN = MM2;
XX1 = [uout(1,NN),uout(3,NN)];
YY1 = [uout(2,NN),uout(4,NN)];
rr = 0.04;
plot(XX1,YY1,'r--','linewidth',2), hold on
circle(XX1(1),YY1(1),rr,'w')
circle(XX1(2),YY1(2),rr,'w')
MM3 = 3*MM/4; MM3 = fix(MM3);
NN = MM3;
XX1 = [uout(1,NN),uout(3,NN)];
YY1 = [uout(2,NN),uout(4,NN)];
rr = 0.04;
plot(XX1,YY1,'g--','linewidth',2), hold on
circle(XX1(1),YY1(1),rr,'w')
circle(XX1(2),YY1(2),rr,'w')
NN = MM;
XX1 = [uout(1,NN),uout(3,NN)];
YY1 = [uout(2,NN),uout(4,NN)];
rr = 0.04;
plot(XX1,YY1,'c--','linewidth',2), hold on
circle(XX1(1),YY1(1),rr,'w')
circle(XX1(2),YY1(2),rr,'w')
text(0.9,-2.2,'t = 0','fontsize',18)
text(-2.3,-1.8,'t = T/4','fontsize',18)
text(0.2,-2.7,'t = T/2','fontsize',18)
text(1.3,-1.7,'t = 3T/4','fontsize',18)
text(-0.8,-2.5,'t = T','fontsize',18)

grid off
axis equal tight

