function bild08
% Figure for exact example
% Plots CONTOUR for Z and W and quiver for V
load daten8a p e t IP Parmeter Z0 W0
load daten8b RDZ RCZ RDW
load daten8c W Z
bilda = 100;
while ~ismember(bilda,[1,2])
   bilda = input(' Welches Bild?, (1/2) ');
end
for I = 1:size(e,2)
   A = [p(1,e(1,I));p(1,e(2,I))];
   B = [p(2,e(1,I));p(2,e(2,I))];
   plot(A,B,'r','linewidth',2), hold on
end
axis equal, axis  manual, grid on
X = p(1,:); Y = p(2,:); Z1 = zeros(1,length(X));
xlin    = linspace(min(X),max(X),20);
ylin    = linspace(min(Y),max(Y),20);
[U1,V1] = meshgrid(xlin,ylin);
W1      = griddata(X,Y,Z1,U1,V1,'v4');
clf, hold on
trimesh(t(1:3,:)',X,Y,Z1,'edgecolor','y'), hold on
switch bilda
case 1, disp(' Contour for Z and Z exakt')
   W1 = griddata(X,Y,Z,U1,V1,'v4');
   %[C,h] = contour(U1,V1,W1,10); hold on
   [C,h] = contour(U1,V1,W1,[-0.02,-0.05,-0.1,-0.15,-0.20,-0.25],'r'); hold on
   clabel(C,h,'labelspacing',1000)
   W1    = griddata(X,Y,Z0,U1,V1,'v4');
   [C,h] = contour(U1,V1,W1,[-0.02,-0.05,-0.1,-0.15,-0.20,-0.25],'k--'); hold on
case 2, disp(' Contour for W and W exakt ')
   W1    = griddata(X,Y,W',U1,V1,'v4');
   %[C,h] = contour(U1,V1,W1,5); hold on
   [C,h] = contour(U1,V1,W1,[-6,-4,-2,0,2,4,6],'r'); hold on
   clabel(C,h,'labelspacing',800)
   W1    = griddata(X,Y,W0,U1,V1,'v4');
   [C,h] = contour(U1,V1,W1,[-6,-4,-2,0,2,4,6],'k--'); hold on
end
axis equal, grid off
%clear
