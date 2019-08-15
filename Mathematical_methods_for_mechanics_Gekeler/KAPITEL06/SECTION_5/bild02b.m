function bild02b
% Alternative figure for DEMO2.M
% in rotated coordinate system
clf
load daten2  T X V Y Parmeter example
rr  = Parmeter(4);
c   = 0.05; d = 0.05;
N = size(Y,1);
[PHI,RR] = cart2pol(Y(:,2),Y(:,5));
% -- turning back ------------------
U = [Y(:,3)';Y(:,6)']; V = zeros(2,N);
for I = 1:N
   C = [cos(PHI(I)), sin(PHI(I)); -sin(PHI(I)), cos(PHI(I))];
   V(:,I) = C*U(:,I);
end
plot(V(1,:),V(2,:),'k','linewidth',2),hold on
circle(X(1,1),X(2,1),rr,'r')
circle(X(1,2),X(2,2),rr,'b')
circle(X(1,3),X(2,3),rr,'k')
circle(V(1,N),V(2,N),rr,'k')

control = 0;
if control == 1
   U = [Y(:,1)';Y(:,4)']; V = zeros(2,N);
   for I = 1:N
      C = [cos(PHI(I)), sin(PHI(I)); - sin(PHI(I)), cos(PHI(I))];
      V(:,I) = C*U(:,I);
   end
   plot(V(1,:),V(2,:),'g.','markersize',12),hold on
   circle(V(1,N),V(2,N),rr,'y'),hold on
end
% -- Corners of figure -------------
l = Parmeter(5); u = Parmeter(6);
r = Parmeter(7); o = Parmeter(8);
XR = [l,r,r,l,l]; YR = [u,u,o,o,u];
plot(XR,YR,'k','linewidth',2),hold on
ee = 0.1;
plot(l-ee,u-ee,'.','color','w','markersize',3)
plot(r+ee,o+ee,'.','color','w','markersize',3)
grid on
% --- Text ------------------------------
if example  == 1
  % circle(0,0,rr,'w')
   text(-0.7,0.7,'Earth','fontsize',22)
   text(0.4,-0.7,'Moon','fontsize',22)
   cc = 0.07; dd = 0.03;
   X = [0.6,0.95]; Y = [-0.6,-0.1];
   arrow(X,Y,cc,dd,'k',1)
   X = [-0.5,-0.05]; Y = [0.6,0.05];
   arrow(X,Y,cc,dd,'k',1)
end
axis equal tight
grid off
axis off
