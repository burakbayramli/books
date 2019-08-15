function bild03
% Figure for flow past cylinder
% Plots CONTOUR for Z and W and quiver for V
clc, clear
load daten3a p e t RAND 
load daten3b NACHBAR NORMALEN
load daten3c W Z
V = velocity(p,e,t,Z);

bilda = 100;
while ~ismember(bilda,[1,2,3])
   bilda = input(' Which figure?, (1/2/3) ');
end
%bilda = 1;
% -- Eckpunkte ---------------------
clf, hold on
plot(0,0,'w.'), hold on
plot(20,10,'w.'), hold on
axis equal tight, axis manual, grid off
% -- Rand ----------------------
for I = 1:size(RAND,2)
   A = [p(1,RAND(1,I));p(1,RAND(2,I))];
   B = [p(2,RAND(1,I));p(2,RAND(2,I))];
   plot(A,B,'r','linewidth',2), hold on
end
TEXT = 0;
if TEXT == 1
   text(9,11,'(A)','fontsize',18)
   text(9,-1,'(C)','fontsize',18)
   text(-1.8,5,'(B)','fontsize',18)
   text(20.3,5,'(D)','fontsize',18)
   text(4.5,5,'(E)','fontsize',12)
end
X = p(1,:); Y = p(2,:); Z1 = zeros(1,length(X));
xlin    = linspace(min(X),max(X),20);
ylin    = linspace(min(Y),max(Y),20);
[U1,V1] = meshgrid(xlin,ylin);
W1      = griddata(X,Y,Z1,U1,V1,'cubic');
trimesh(t(1:3,:)',X,Y,Z1,'edgecolor','y'), hold on

switch bilda
case 1, disp(' Contour for Z')
   W1 = griddata(X,Y,Z,U1,V1,'cubic');
   %[C,h] = contour(U1,V1,W1,10);
   %clabel(C,h,'labelspacing',1000);
   [C,h] = contour(U1,V1,W1,[-4,-3,-2,-1,-0.5,0.5,1,2,3,4],'k','linewidth',1);
   %clabel(C,h,'labelspacing',800)
   %clabel(C,h,'manual','fontsize',15)
   [C,h] = contour(U1,V1,W1,[-0.1,-0.05,-0.01,-0.005],'r');
   %clabel(C,h,'labelspacing',800)
   [C,h] = contour(U1,V1,W1,[0.05,0.1,0.01],'b');
   %clabel(C,h,'labelspacing',800)

   %clabel(C,h,'manual','fontsize',10)
case 2, disp(' Contour for W ')
   W1    = griddata(X,Y,W',U1,V1,'cubic');
   %[C,h] = contour(U1,V1,W1,10); hold on
   %clabel(C,h,'labelspacing',1000)
   [C,h] = contour(U1,V1,W1,[-0.7,-0.3,-0.1],'k'); hold on
   [C,h] = contour(U1,V1,W1,[0.7,0.3,0.1],'r'); hold on
   [C,h] = contour(U1,V1,W1,[-1.4,-1,1,1.4],'g'); hold on

case 3, disp(' Quiver fuer V ')
%   XM = sum(p(1,t(1:3,:)),1)/3; % fails
%   YM = sum(p(2,t(1:3,:)),1)/3;
   axis([0 10 0 10])
   M = size(t,2); XM = zeros(1,M); YM = XM;
   for I = 1:M
       XM(I) = sum(p(1,t(1:3,I)))/3;
       YM(I) = sum(p(2,t(1:3,I)))/3;
   end
   quiver(XM,YM,V(1,:),V(2,:))
end
% grid off
%clear
