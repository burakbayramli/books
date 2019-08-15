function bild01
% Figure for lid driven cavity, contour plot
load daten1a p e t RAND  
load daten1b P W Z
clc
Z = 100*Z; % !!!!!!!!!!!!!!
bilda = 100;
while ~ismember(bilda,[1,2,3])
   bilda = input(' Which figure?, (1/2/3) ');
end
%bilda = 3;
% -- Eckpunkte
clf, hold on % fuer flaches Bild
plot(-0.01,-0.01,'w.'), hold on
plot(1.01,1.01,'w.'), hold on
% -- Rand -------------------------
for I = 1:size(RAND,2)
   A = [p(1,RAND(1,I));p(1,RAND(2,I))];
   B = [p(2,RAND(1,I));p(2,RAND(2,I))];
   plot(A,B,'r','linewidth',2), hold on
end
axis equal tight, axis  manual, grid on
X = p(1,:); Y = p(2,:); Z1 = zeros(1,length(X));
%trimesh(t(1:3,:)',X,Y,Z1,'edgecolor','y'), hold on

xlin    = linspace(min(X),max(X),20);
ylin    = linspace(min(Y),max(Y),20);
[U1,V1] = meshgrid(xlin,ylin);
switch bilda
case 1, disp(' Contour fuer Z ')
W1 = griddata(X,Y,Z,U1,V1,'cubic');
%[C,h] = contour(U1,V1,W1,20,'k');
%clabel(C,h,'labelspacing',1000)

[C,h] = contour(U1,V1,W1,[-10,-8,-6,-4,-2,-1,-0.05],'k','linewidth',2); hold on
clabel(C,h,'labelspacing',1000)
%clabel(C,h,'manual','fontsize',20)
[C,h] = contour(U1,V1,W1,[-0.01,-0.02,0],'k'); hold on
[C,h] = contour(U1,V1,W1,[0.002,0.004,0.006,0.008],'k'); hold on
[C,h] = contour(U1,V1,W1,[0.01,0.02,0.03,0.04,0.05],'k'); hold on
%clabel(C,h,'labelspacing',1000)
%%clabel(C,h,'manual','fontsize',20)
%text(0.43,0.93,'(A)','fontsize',24)
%text(0.02,0.5,'(B)','fontsize',24)
%text(0.43,0.07,'(C)','fontsize',24)
%text(0.85,0.5,'(D)','fontsize',24)
%
case 2, disp(' contour for W ')
   W1    = griddata(X,Y,W',U1,V1,'cubic');
   [C,h] = contour(U1,V1,W1,[-5,-4,-3,-2,-1,0,1,2,5],'k','linewidth',2);
   hold on
   clabel(C,h,'labelspacing',1000)
   %clabel(C,h,'manual','fontsize',20)
case 3, disp(' contour for P ')
   P = 100*P;
   W1    = griddata(X,Y,P',U1,V1,'cubic');
   [C,h] = contour(U1,V1,W1,[-8,-5,-4,-2,-1,0,1,2,3,4,10],'k','linewidth',2);
   hold on
   %clabel(C,h,'manual','fontsize',20)
   clabel(C,h,'labelspacing',1000,'fontsize',20)
end
grid off, axis off
clear
