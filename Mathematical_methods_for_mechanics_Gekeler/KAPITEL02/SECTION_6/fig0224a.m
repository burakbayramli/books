function fig0224a
% Figure 2.24
clf
load datend m XA Periode XANF
% -- X-Axis -------------
c = 0.2; d = 0.15;
X1 = [-3, 3]; Y1 = [0,0];
arrow(X1,Y1,c,d,'k',2), hold on
% -- Y-Axis --------------
c = 0.4; d = 0.1;
X1 = [0,0]; Y1 = [-4,4];
arrow(X1,Y1,c,d,'k',2), hold on
% -- Axis of period -------
X1 = [Periode,Periode];
Y1 = [-2,2]; 
%plot(X1,Y1,'--','color','k'), hold on
plot(XA(1,:),XA(2,:),'k','linewidth',2), hold on
plot(XANF(:,1),XANF(:,2),'k:','linewidth',2), hold on
plot(XANF(1,1),XANF(1,2),'ko','markersize',6)

%Periode
axis tight

