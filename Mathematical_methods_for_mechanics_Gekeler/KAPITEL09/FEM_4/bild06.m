function bild06
% Plots CONTOUR for Z and W and quiver for V
%clc
bilda = 100;
while ~ismember(bilda,[1,2,3])
   bilda = input(' Welches Bild?, (1/2/3) ');
end   
%bilda = 2;
load daten6a p e t RAND IP T_EXACT Z_EXACT W_EXACT
load daten6b RDZ RCZ RDW RDT RCT
load daten6c V W Z T

X = p(1,:); Y = p(2,:); Z1 = zeros(1,length(X));
xlin    = linspace(min(X),max(X),20);
ylin    = linspace(min(Y),max(Y),20);
[U1,V1] = meshgrid(xlin,ylin);
W1      = griddata(X,Y,Z1,U1,V1,'v4');
clf, hold on
%trimesh(t(1:3,:)',X,Y,Z1,'edgecolor','y'), hold on
for I = 1:size(RAND,2)
   A = [p(1,RAND(1,I));p(1,RAND(2,I))];
   B = [p(2,RAND(1,I));p(2,RAND(2,I))];
   plot(A,B,'r','linewidth',2), hold on
end
axis equal, axis  manual, grid on
switch bilda
case 1, disp(' Contour for T and T_EXACT ')
   W1    = griddata(X,Y,T,U1,V1,'v4');
   [C,h] = contour(U1,V1,W1,5); hold on
   %[C,h] = contour(U1,V1,W1,[-6,-4,-2,0,2,4,6],'r'); hold on
   %clabel(C,h,'labelspacing',800)
   W1    = griddata(X,Y,T_EXACT',U1,V1,'v4');
   [C,h] = contour(U1,V1,W1,5,'k'); hold on
%   [C,h] = contour(U1,V1,W1,[-6,-4,-2,0,2,4,6],'k'); hold on
  % clabel(C,h,'color','k','labelspacing',1000)
case 2, disp(' Contour for Z and Z_EXACT ')
   W1 = griddata(X,Y,Z,U1,V1,'v4');
   [C,h] = contour(U1,V1,W1,10); hold on
   %[C,h] = contour(U1,V1,W1,[-0.02,-0.05,-0.1,-0.15,-0.20,-0.25]); hold on
%   clabel(C,h,'labelspacing',1000)
   W1    = griddata(X,Y,Z_EXACT',U1,V1,'v4');
   [C,h] = contour(U1,V1,W1,10,'k'); hold on
   %[C,h] = contour(U1,V1,W1,[-0.02,-0.05,-0.1,-0.15,-0.20,-0.25],'k'); hold on
   % clabel(C,h,'color','k','labelspacing',1000)
case 3, disp(' Contour for W and W_EXACT ')
   W1    = griddata(X,Y,W,U1,V1,'v4');
   [C,h] = contour(U1,V1,W1,5); hold on
   %[C,h] = contour(U1,V1,W1,[-6,-4,-2,0,2,4,6],'r'); hold on
   %clabel(C,h,'labelspacing',800)
   W1    = griddata(X,Y,W_EXACT',U1,V1,'v4');
   [C,h] = contour(U1,V1,W1,5,'k'); hold on
%   [C,h] = contour(U1,V1,W1,[-6,-4,-2,0,2,4,6],'k'); hold on
  % clabel(C,h,'color','k','labelspacing',1000)
end
grid off
%clear
