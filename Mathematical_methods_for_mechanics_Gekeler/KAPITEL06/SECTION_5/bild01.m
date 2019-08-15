function bild01
% Figure for DEMO1.M
clf
load daten1 T X Y Parmeter
rr  = 0.03; c   = 0.05; d = 0.05;
plot(Y(:,1),Y(:,2)),hold on
circle(0,0,rr,'k'), circle(X(1),X(2),rr,'r')
% -- Corners of Figure ---------
%plot(-0.2,-0.4,'.','color','w','markersize',3)
%plot(1.1,0.4,'.','color','w','markersize',3)
grid on
axis equal tight
%grid off
%axis off
