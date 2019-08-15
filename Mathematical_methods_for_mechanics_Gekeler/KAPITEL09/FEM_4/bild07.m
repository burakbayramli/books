function bild07
% Plots CONTOUR for Z and W and quiver for V
%clc
bilda = 100;
while ~ismember(bilda,[1,2,3])
   bilda = input(' Welches Bild?, (1/2/3) ');
end
%bilda = 2;
load daten7a p e t RAND NACHBAR NORMALEN 
load daten7b RDZ RCZ RDW RDT RCT
load daten7c T_EXACT Z_EXACT W_EXACT DELTA_T CONVEC_T %DELTA_W CONVEC_W
load daten7d V W Z T

X = p(1,:); Y = p(2,:); Z1 = zeros(1,length(X));
xlin    = linspace(min(X),max(X),20);
ylin    = linspace(min(Y),max(Y),20);
[U1,V1] = meshgrid(xlin,ylin);
W1      = griddata(X,Y,Z1,U1,V1,'v4');
clf, hold on
trimesh(t(1:3,:)',X,Y,Z1,'edgecolor','y'), hold on
for I = 1:size(RAND,2)
   A = [p(1,RAND(1,I));p(1,RAND(2,I))];
   B = [p(2,RAND(1,I));p(2,RAND(2,I))];
   plot(A,B,'r','linewidth',2), hold on
end
axis equal, axis  manual, grid on
switch bilda

case 1, disp(' Contour for T und T_EXACT')
   FAC = 1E4; %%!!!!!!!!!!!!!!!!!!
   T = FAC*T; T_EXACT = FAC*T_EXACT;
   W1    = griddata(X,Y,T,U1,V1,'v4');
   [C,h] = contour(U1,V1,W1,[-15,-12,-9,-6,-3],'r'); hold on
   clabel(C,h,'labelspacing',1000)
   W1    = griddata(X,Y,T_EXACT',U1,V1,'v4');
   [C,h] = contour(U1,V1,W1,[-15,-12,-9,-6,-3],'k'); hold on
   clabel(C,h,'labelspacing',1000)

case 2, disp(' Contour for Z und Z_EXACT')
   FAC = 1E3; %%!!!!!!!!!!!!!!!!!!
   Z = FAC*Z; Z_EXACT = FAC*Z_EXACT;
   W1 = griddata(X,Y,Z,U1,V1,'v4');
   [C,h] = contour(U1,V1,W1,[-3,-5,-10,-15,-20,-25],'r'); hold on
   clabel(C,h,'labelspacing',1000)
   W1    = griddata(X,Y,Z_EXACT',U1,V1,'v4');
   [C,h] = contour(U1,V1,W1,[-3,-5,-10,-15,-20,-25],'k'); hold on
   clabel(C,h,'labelspacing',1000)

case 3, disp(' Contour for W und W_EXACT')
   FAC = 1E1; %%!!!!!!!!!!!!!!!!!!
   W = FAC*W; W_EXACT = FAC*W_EXACT;
   W1    = griddata(X,Y,W,U1,V1,'v4');
   [C,h] = contour(U1,V1,W1,[-8,-6,-4,-2,0,2,4,6,8],'r'); hold on
   clabel(C,h,'labelspacing',1000)
   W1    = griddata(X,Y,W_EXACT',U1,V1,'v4');
   [C,h] = contour(U1,V1,W1,[-8,-6,-4,-2,0,2,4,6,8],'k'); hold on
   clabel(C,h,'labelspacing',1000)
end
grid off
%clear
