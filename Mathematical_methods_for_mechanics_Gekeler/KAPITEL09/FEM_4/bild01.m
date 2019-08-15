function bild01
% Image for thermal flow in a cup
% Plots CONTOUR for Z, T, W, and quiver for V
clc, clear
load daten1a p e t RAND Parmeter
load daten1b RDZ RDW RDT RCT NACHBAR NORMALEN
load daten1c V W Z T
bilda = 100;
while ~ismember(bilda,[1,2,3,4])
   bilda = input(' Welches Bild?, (1/2/3/4) ');
end
clf, hold on
X = p(1,:); Y = p(2,:); Z1 = zeros(1,length(X));
xlin    = linspace(min(X),max(X),30);
ylin    = linspace(min(Y),max(Y),30);
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
   W1    = griddata(X,Y,T,U1,V1,'v4'); %'v4'  oder 'cubic'
   [C,h] = contour(U1,V1,W1,[55,50,45,40,35,30,25,20,15]); hold on
   clabel(C,h,'labelspacing',1000)
   
case 2, disp(' Contour for Z mal 1.0E3 ')
   Z = 1.0E3*Z;
   W1 = griddata(X,Y,Z,U1,V1,'v4');
   [C,h] = contour(U1,V1,W1,[-25,-20,-15,-10,-5,0,5,10,15,20,25],'r'); hold on
   clabel(C,h,'labelspacing',1000)
   
case 3, disp(' Contour for W mal 10')
   W = 10*W;
   W1    = griddata(X,Y,W',U1,V1,'v4');
   [C,h] = contour(U1,V1,W1,[-6,-4,-2,0,2,4,6],'r'); hold on
   clabel(C,h,'labelspacing',800)
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
clear
