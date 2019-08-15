function bild03
% Image for convection in a box, contour plot
% with notations
clc, clear
load daten3a p e t RAND Parmeter
load daten3b RDZ RDW RDT RCT NACHBAR 
load daten3c W Z T P Parmeter1
bilda = 100;
while ~ismember(bilda,[1,2,3,4])
   bilda = input(' Welches Bild?, (1/2/3/4) ');
end
%bilda = 1;
% -- Eckpunkte ------------------
clf, hold on
nodes = size(p,2)
% -- Trimesh und Rand --------------------
X = p(1,:); Y = p(2,:); Z1 = zeros(1,length(X));
xlin    = linspace(min(X),max(X),20);
ylin    = linspace(min(Y),max(Y),20);
[U1,V1] = meshgrid(xlin,ylin);
W1      = griddata(X,Y,Z1,U1,V1,'cubic');
%trimesh(t(1:3,:)',X,Y,Z1,'edgecolor','g'), hold on
for I = 1:size(RAND,2)
   A = [p(1,RAND(1,I));p(1,RAND(2,I))];
   B = [p(2,RAND(1,I));p(2,RAND(2,I))];
   plot(A,B,'r','linewidth',2), hold on
end
axis equal, axis manual, grid on

% ----------------------------------
switch bilda
case 1, disp(' contour for T ')
   W1    = griddata(X,Y,T,U1,V1,'cubic');
   % [C,h] = contour(U1,V1,W1,10,'k'); hold on
   [C,h] = contour(U1,V1,W1,[0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9],'k','linewidth',2);
   hold on
   clabel(C,h,'labelspacing',1000)
   % clabel(C,h,'manual','fontsize',22)
   % text(0.43,0.93,'(A)','fontsize',24)
   % text(0.02,0.5,'(B)','fontsize',24)
   % text(0.43,0.07,'(C)','fontsize',24)
   % text(0.85,0.5,'(D)','fontsize',24)
case 2, disp(' Contour for Z ')
   W1 = griddata(X,Y,Z,U1,V1,'cubic');
   %  [C,h] = contour(U1,V1,W1,10,'k'); hold on
   [C,h] = contour(U1,V1,W1,[2,4,6,8,9.5,10,10.2],'k','linewidth',2); hold on
   clabel(C,h,'manual','fontsize',20)
   %clabel(C,h,'labelspacing',1000)
case 3, disp(' Contour for W ')
   W1    = griddata(X,Y,W',U1,V1,'cubic');
%   [C,h] = contour(U1,V1,W1,10,'k'); hold on
   [C,h] = contour(U1,V1,W1,[-800,-400,-200,0,200,100,400,800],'k','linewidth',2);
    hold on
    %clabel(C,h,'labelspacing',1000)
     clabel(C,h,'manual','fontsize',20)
case 4, disp(' contour for P ')
   P = P/100;
   W1    = griddata(X,Y,P',U1,V1,'cubic');
   [C,h] = contour(U1,V1,W1,[-14,-10,-8,-6,-4,-2,0,2,4,6,8,10,14],'k','linewidth',2);
    hold on
   %[C,h] = contour(U1,V1,W1,[0,10,20],'r','linewidth',2); hold on
    clabel(C,h,'labelspacing',1000,'fontsize',18)
   %[C,h] = contour(U1,V1,W1,[-125,-100,-75,-50,-25],'k','linewidth',2); hold on
   clabel(C,h,'labelspacing',1000)
   %clabel(C,h,'manual','fontsize',20)
end
grid off
%clear
  