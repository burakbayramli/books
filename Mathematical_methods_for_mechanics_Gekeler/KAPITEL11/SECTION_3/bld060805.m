function bld060805
% Bewegungskurven  fuer Roboter nach Schiehlen

load daten T Y Parmeter
clf
subplot(3,1,1)
plot(T,Y(:,1),'k')
text(0.05,4,'Z1 [m]','fontsize',18)
text(1.8,2.6,'t [s]','fontsize',18)
axis tight
subplot(3,1,2)
plot(T,Y(:,2),'k')
text(0.05,-0.57,'GA1 [rad]','fontsize',18)
text(1.8,-0.7,'t [s]','fontsize',18)
axis tight
subplot(3,1,3)
plot(T,Y(:,3),'k')
text(0.05,3.2,'Y2 [m]','fontsize',18)
text(1.8,1.2,'t [s]','fontsize',18)
axis tight

