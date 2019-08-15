function fig0948
% Figure 9.48

load daten p e t 
clf, hold on
X = p(1,:); Y = p(2,:); Z1 = zeros(1,length(X));
xlin  = linspace(min(X),max(X),30);
ylin  = linspace(min(Y),max(Y),30);
[U,V] = meshgrid(xlin,ylin);
W     = griddata(X,Y,Z1,U,V,'cubic');
trimesh(t(1:3,:)',X,Y,Z1,'edgecolor','k'), hold on
for I = 1:size(e,2)
   A = [p(1,e(1,I));p(1,e(2,I))];
   B = [p(2,e(1,I));p(2,e(2,I))];
   plot(A,B,'k','linewidth',2), hold on
end
  
LU = [min(X),min(Y)]; LU = LU - 0.4;
RO = [max(X),max(Y)]; RO = RO + 0.4;
RO(1)= RO(1) + 0.2;
plot(LU(1),LU(2),'w.'), hold on
plot(RO(1),RO(2),'w.'), hold on
axis equal tight, axis manual, %grid off
ss = 18;
text(0.1,-0.2,'1','fontsize',ss)
text(1.1,-0.2,'2','fontsize',ss)
text(2.1,-0.2,'5','fontsize',ss)

text(0.1,0.8,'3','fontsize',ss)
text(1.1,0.8,'4','fontsize',ss)
text(2.2,0.8,'8','fontsize',ss)

text(0.1,1.8,'6','fontsize',ss)
text(1.1,1.8,'7','fontsize',ss)
text(1.7,1.8,'9','fontsize',ss)
text(2.7,1.7,'12','fontsize',ss)

text(0.1,2.8,'10','fontsize',ss)
text(1.1,2.8,'11','fontsize',ss)
text(1.9,2.8,'14','fontsize',ss)
text(3.2,2.2,'18','fontsize',ss)
text(2.95,2.95,'21','fontsize',ss)
text(3.9,2.6,'22','fontsize',ss)

text(0.1,3.8,'13','fontsize',ss)
text(1.1,3.8,'15','fontsize',ss)
text(2.1,3.8,'19','fontsize',ss)
text(3.1,3.8,'23','fontsize',ss)
text(4.1,3.8,'26','fontsize',ss)
text(5.1,3.8,'28','fontsize',ss)
text(5.1,2.8,'25','fontsize',ss)

text(0.1,4.8,'16','fontsize',ss)
text(1.1,4.8,'17','fontsize',ss)
text(2.1,4.8,'20','fontsize',ss)
text(3.1,4.8,'24','fontsize',ss)
text(4.1,4.8,'27','fontsize',ss)
text(5.1,4.8,'29','fontsize',ss)

%grid on
