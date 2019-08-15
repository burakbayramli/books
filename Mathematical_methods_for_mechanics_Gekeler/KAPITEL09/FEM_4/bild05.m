function bild05
% Figure for convection in a unit square box
% Plots CONTOUR for Z, T,W, P
clc, clear
load daten5a p e t RAND IP
load daten5b RDZ RCZ RDW RDT RCT
load daten5c W Z T P Parmeter1
% disp(' Ev. Werte von Z, W abaendern ! ')
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
bilda = 100;
while ~ismember(bilda,[1,2,3,4])
   bilda = input(' Welches Bild?, (1/2/3/4) ');
end
clf, hold on % fuer flaches Bild
X = p(1,:); Y = p(2,:); Z1 = zeros(1,length(X));
xlin    = linspace(min(X),max(X),20);
ylin    = linspace(min(Y),max(Y),20);
[U1,V1] = meshgrid(xlin,ylin);
W1      = griddata(X,Y,Z1,U1,V1,'cubic');
trimesh(t(1:3,:)',X,Y,Z1,'edgecolor','y'), hold on
for I = 1:size(RAND,2)
   A = [p(1,RAND(1,I));p(1,RAND(2,I))];
   B = [p(2,RAND(1,I));p(2,RAND(2,I))];
   plot(A,B,'r','linewidth',2), hold on
end
axis equal , axis  manual, grid off
switch bilda
case 1, disp(' Contour for T ')
   W1    = griddata(X,Y,T,U1,V1,'v4');
   [C,h] = contour(U1,V1,W1,10); hold on
  % [C,h] = contour(U1,V1,W1,[1,2,3,4,5,6,7,8,9]); hold on
   clabel(C,h,'labelspacing',1000)
case 2, disp(' Contour for Z ')
   W1 = griddata(X,Y,Z,U1,V1,'v4');
   [C,h] = contour(U1,V1,W1,10); hold on
   %[C,h] = contour(U1,V1,W1,[36,32,28,24,20,16,12,8,4,1]); hold on
   clabel(C,h,'labelspacing',1000)
  % [C,h] = contour(U1,V1,W1,[50,60,70]); hold on
case 3, disp(' Contour for W ')
   W1    = griddata(X,Y,W',U1,V1,'v4');
   [C,h] = contour(U1,V1,W1,10); hold on
   % [C,h] = contour(U1,V1,W1,[-800,-400,-200,0,200,400,600,800]); hold on
   clabel(C,h,'labelspacing',1000)
case 4, disp(' contour for P ')
   P = P/100;
   W1    = griddata(X,Y,P',U1,V1,'cubic');
   [C,h] = contour(U1,V1,W1,10,'k','linewidth',2); hold on
   %[C,h] = contour(U1,V1,W1,[0,10,20],'r','linewidth',2); hold on
     % clabel(C,h,'labelspacing',1000)
  % [C,h] = contour(U1,V1,W1,[-125,-100,-75,-50,-25],'k','linewidth',2); hold on
   clabel(C,h,'labelspacing',1000)
   %clabel(C,h,'manual','fontsize',20)
end
%clear
