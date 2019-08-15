function fig0224
% Figure 2.24
clf
load datend m XA Periode
aa = 0.1;
XMAX = max(abs(XA(1,:)))+ aa;
YMAX = max(abs(XA(2,:)))+aa;
% -- X-Axis -------------
X1 = [-XMAX, XMAX]; Y1 = [0,0];
plot(X1,Y1,'k','linewidth',2), hold on
% -- Y-Axis --------------
c = 0.4; d = 0.15;
X1 = [0,0]; Y1 = [-YMAX,YMAX];
plot(X1,Y1,'k','linewidth',2), hold on
% -- Axis of period -------
X1 = [Periode,Periode];
Y1 = [-2,2]; 
plot(XA(1,:),XA(2,:),'k','linewidth',1), hold on
plot(XA(1,1),XA(2,1),'ko','markersize',6), hold on
plot(XA(1,end),XA(2,end),'k*','markersize',6), hold on

%Periode
axis tight

