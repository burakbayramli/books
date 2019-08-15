function bild02
% Image for convection in a compartment
% Plots CONTOUR for Z, T, W, and quiver for V
%clc
format short e
load daten2a p e t RAND Parmeter
load daten2b RDZ RCT RDW RDT RCT NACHBAR NORMALEN
load daten2c V W Z T
bilda = 100;
while ~ismember(bilda,[1,2,3])
   bilda = input(' Welches Bild?, (1/2/3/4) ');
end
%bilda = 1;
clf
X = p(1,:); Y = p(2,:); Z1 = zeros(1,length(X));
hold on  % fuer flaches Bild --------
xlin    = linspace(min(X),max(X),20);
ylin    = linspace(min(Y),max(Y),20);
[U1,V1] = meshgrid(xlin,ylin);
W1      = griddata(X,Y,Z1,U1,V1,'v4');
trimesh(t(1:3,:)',X,Y,Z1,'edgecolor','g'), hold on
for I = 1:size(RAND,2)
   A = [p(1,RAND(1,I));p(1,RAND(2,I))];
   B = [p(2,RAND(1,I));p(2,RAND(2,I))];
   plot(A,B,'r','linewidth',2), hold on
end
axis equal, axis  manual
switch bilda
case 1, disp(' Contour for T ')
   W1    = griddata(X,Y,T,U1,V1,'v4');
 %  [C,h] = contour(U1,V1,W1,[20.5,21,21.5,22,22.5]); hold on
 %  clabel(C,h,'labelspacing',1000)
    [C,h] = contour(U1,V1,W1,10); hold on

case 2, disp(' Contour for Z ')
   W1 = griddata(X,Y,Z,U1,V1,'v4');
   [C,h] = contour(U1,V1,W1,10); hold on
   clabel(C,h,'labelspacing',1000)
case 3, disp(' Contour for W ')
   W1    = griddata(X,Y,W',U1,V1,'v4');
   [C,h] = contour(U1,V1,W1,5); hold on
   clabel(C,h,'labelspacing',1000)
case 4, disp(' Quiver for V ')
   U1 = V(1,:); V1 = V(2,:);
%   XM = sum(p(1,t(1:3,:)),1)/3; FAILS SOMETIMES
%   YM = sum(p(2,t(1:3,:)),1)/3;
   M = size(t,2); XM = zeros(1,M); YM = XM;
   for I = 1:M
       XM(I) = sum(p(1,t(1:3,I)))/3;
       YM(I) = sum(p(2,t(1:3,I)))/3;
   end
   quiver(XM,YM,U1,V1)
end
%clear
