function bild14
% Image for DEMO12.M

load daten12a p e q 
load daten12c W Z
bilda = 100;
%while ~ismember(bilda,[1,2])
%   bilda = input(' Which figure?, (1/2) ');
%end
bilda = 1;
Nodenumber = size(p,2)
Z = 100*Z; %!!!!!!!!!!!!!!!!!!!!!
clf, hold on
mesh36(p,q,'g'), hold on
X = p(1,:); Y = p(2,:); Z1 = zeros(1,length(X));
xlin    = linspace(min(X),max(X),20);
ylin    = linspace(min(Y),max(Y),20);
[U1,V1] = meshgrid(xlin,ylin);
axis equal, axis  manual
switch bilda
case 1, disp(' Contour for Z ')
   W1 = griddata(X,Y,Z,U1,V1,'cubic');
   [C,h] = contour(U1,V1,W1,[-10,-9,-8,-6,-4,-2,-1,-0,-0.05],'k','linewidth',2);
   hold on
   clabel(C,h,'labelspacing',1000)
   %clabel(C,h,'manual','fontsize',22)
   [C,h] = contour(U1,V1,W1,[0.02,0.04,0.06,0.08,0.1],'g'); hold on
   [C,h] = contour(U1,V1,W1,[0.002,0.004,0.006,0.008,0.01],'r'); hold on
   %clabel(C,h,'labelspacing',500)
case 2   
   W1    = griddata(X,Y,W',U1,V1,'v4');
   [C,h] = contour(U1,V1,W1,[-5,-3,-1,0,1,3,5],'k','linewidth',2); hold on
   clabel(C,h,'labelspacing',800)
   %clabel(C,h,'manual','fontsize',26)
end
%clear
