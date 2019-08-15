function bild07
% Figure for lid driven cavity
% Plots CONTOUR for Z and W and quiver for V
%clc
load daten7a p e t Parmeter
load daten7b RDZ RCZ RDW
load daten7c  W Z
bilda = 100;
while ~ismember(bilda,[1,2])
   bilda = input(' Welches Bild?, (1/2) ');
end
%bilda = 1;
Nodenumber = size(p,2)
Z = 100*Z; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% -- Eckpunkte
clf, hold on % fuer flaches Bild
%plot(-0.2,-0.2,'w.'), hold on
%plot(1.2,1.2,'w.'), hold on
%axis equal, axis  manual, grid on
for I = 1:size(e,2)
   A = [p(1,e(1,I));p(1,e(2,I))];
   B = [p(2,e(1,I));p(2,e(2,I))];
   plot(A,B,'r','linewidth',2), hold on
end
axis equal, axis  manual,grid on
X = p(1,:); Y = p(2,:); Z1 = zeros(1,length(X));
xlin    = linspace(min(X),max(X),20);
ylin    = linspace(min(Y),max(Y),20);
[U1,V1] = meshgrid(xlin,ylin);
W1      = griddata(X,Y,Z1,U1,V1,'cubic');
trimesh(t(1:3,:)',X,Y,Z1,'edgecolor','y'), hold on
switch bilda
case 1, disp(' Contour for Z ')
   W1 = griddata(X,Y,Z,U1,V1,'v4');
   [C,h] = contour(U1,V1,W1,[-10,-9,-8,-6,-4,-2,-1,-0,-0.05],'k','linewidth',2);
   hold on
   clabel(C,h,'labelspacing',1000)
   %clabel(C,h,'manual','fontsize',22)
   [C,h] = contour(U1,V1,W1,[0.02,0.04,0.06,0.08,0.1],'g'); hold on
   [C,h] = contour(U1,V1,W1,[0.002,0.004,0.006,0.008,0.01],'r'); hold on
   %clabel(C,h,'labelspacing',500)
case 2, disp(' Contour for W ')
   W1    = griddata(X,Y,W',U1,V1,'v4');
   [C,h] = contour(U1,V1,W1,[-5,-3,-1,0,1,3,5],'k','linewidth',2); hold on
   clabel(C,h,'labelspacing',800)
   %clabel(C,h,'manual','fontsize',26)
end
grid off
%clear
