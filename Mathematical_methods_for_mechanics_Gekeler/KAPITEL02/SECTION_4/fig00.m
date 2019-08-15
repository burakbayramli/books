function fig00
% Image for Arenstorf orbits
clf
% x = [x; y; xprime; yprime]
load daten xout
mue = 0.012277471;
mue1 = 1- mue
plot(xout(1,:),xout(2,:),'k','linewidth',2), hold on
rr  = 0.02;
circle(0,0,rr,'w')
circle(-mue,0,rr,'r')
circle(mue1,0,rr,'b')
grid on
axis equal tight
%grid off
%axis off
