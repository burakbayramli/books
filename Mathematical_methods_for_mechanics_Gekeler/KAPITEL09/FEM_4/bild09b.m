function bild09b
% Image for transport problem
% Plots CONTOUR for Q, Z, W, and quiver for V
%clc
load daten10a p e RAND t Parmeter XLAENGE YLAENGE
load daten10b RDZ RDW RDQ RDM NACHBAR NORMALEN
load daten10c V W Z Q

bilda = 100;
while ~ismember(bilda,[1,2,3])
   bilda = input(' Welches Bild?, (1/2/3) ');
end   
%bilda = 1;
clf, hold on
X = p(1,:); Y = p(2,:); Z1 = zeros(1,length(X));
xlin    = linspace(min(X),max(X),30);
ylin    = linspace(min(Y),max(Y),30);
[U1,V1] = meshgrid(xlin,ylin);
W1      = griddata(X,Y,Z1,U1,V1,'v4');
%trimesh(t(1:3,:)',X,Y,Z1,'edgecolor','b'), hold on
for I = 1:size(RAND,2)
   A = [p(1,RAND(1,I));p(1,RAND(2,I))];
   B = [p(2,RAND(1,I));p(2,RAND(2,I))];
   plot(A,B,'r','linewidth',2), hold on
end
KK = t(:,19);
fill(p(1,KK),p(2,KK),'g'), hold on
SMOKE = [14.5,7];
SMOKE(1) = XLAENGE*SMOKE(1); SMOKE(2) = YLAENGE*SMOKE(2);
circle(SMOKE(1),SMOKE(2),0.4*XLAENGE,'k')
axis equal, axis  manual

switch bilda
case 1, disp(' Contour fuer Pollution ' )
   W1    = griddata(X,Y,Q,U1,V1,'v4');  % 'linear' FAILS !!!
   [C,h] = contour(U1,V1,W1,[1,2,3,4]); hold on
   clabel(C,h,'labelspacing',1000)
   axis equal, axis  manual
case 2, disp(' Contour fuer Z ')
   W1 = griddata(X,Y,Z,U1,V1,'v4');
   [C,h] = contour(U1,V1,W1,10); hold on
case 3, disp(' Contour fuer W ')
   W1    = griddata(X,Y,W',U1,V1,'v4');
   [C,h] = contour(U1,V1,W1,10); hold on
   %clabel(C,h,'labelspacing',1000)
end
grid on
clear
