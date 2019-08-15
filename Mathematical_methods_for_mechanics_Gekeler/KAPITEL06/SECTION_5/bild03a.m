function bild03a
% figure for DEMO3.M
clf
load daten3 T X V Y Parmeter example
rr  = Parmeter(3);
c   = 0.05; d = 0.05;
plot(Y(:,1),Y(:,3),'r','linewidth',1),hold on
plot(Y(:,2),Y(:,4),'b','linewidth',1),hold on
N = size(Y,1);
circle(X(1,1),X(2,1),rr,'r')
circle(X(1,2),X(2,2),rr,'b')

% -- Corners of Figure ----------
l = Parmeter(4); u = Parmeter(5);
r = Parmeter(6); o = Parmeter(7);
XR = [l,r,r,l,l]; YR = [u,u,o,o,u];
plot(XR,YR,'k','linewidth',2),hold on
ee = 0.1;
plot(l-ee,u-ee,'.','color','w','markersize',3)
plot(r+ee,o+ee,'.','color','w','markersize',3)

grid on
axis equal tight
%grid off
%axis off
