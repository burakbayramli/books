function bild02a
% figure for DEMO2.M
clf
load daten2 T X V Y Parmeter example
rr  = Parmeter(4);
c   = 0.05; d = 0.05;
plot(Y(:,1),Y(:,4),'r','linewidth',1),hold on
plot(Y(:,2),Y(:,5),'b','linewidth',1),hold on
plot(Y(:,3),Y(:,6),'k','linewidth',1),hold on
N = size(Y,1);
circle(X(1,1),X(2,1),rr,'r')
circle(X(1,2),X(2,2),rr,'b')
circle(X(1,3),X(2,3),rr,'k')
circle(Y(N,3),Y(N,6),rr,'k')
if ismember(example,[4,5])
   circle(Y(N,1),Y(N,4),rr,'r')
   circle(Y(N,2),Y(N,5),rr,'b')
end
if example == 2
   circle(0,0,rr,'w')
   text(0.6,0.1,'m_1 = 0.5','fontsize',18)
   text(-1.2,-0.2,'m_3 = 0','fontsize',18)
   text(-1.5,0.8,'m_2 = 0.5','fontsize',18)
   cc = 0.07; dd = 0.03;
   X = [-1.1,-0.55];
   Y = [0.7,0.04];
   arrow(X,Y,cc,dd,'k',1)
end

% -- Corners of Figure ----------
l = Parmeter(5); u = Parmeter(6);
r = Parmeter(7); o = Parmeter(8);
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