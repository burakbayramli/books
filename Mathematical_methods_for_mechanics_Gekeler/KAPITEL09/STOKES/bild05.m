function bild05
% Draws QUIVER for (U,V), CONTOUR for P
% Streamslice for (U,V), Streamline for (U,V)
% in unit square
clf
bilda = 100;
while ~ismember(bilda,[1,2,3])
   bilda = input(' Select figure! (1/2/3) ');
end
load daten5 p e t p1 t1 FU FV U V P

X = p(1,:); Y = p(2,:); Z1 = zeros(1,length(X)); P = P';
% -- exact solution ----------------
UE = X.^3; VE = - 3*X.*X.*Y; PE = X.^3 + Y.^3;

clf, hold on
N1 = size(p,2);
U = U(1:N1); V = V(1:N1);

xlin  = linspace(min(X),max(X),30);
ylin  = linspace(min(Y),max(Y),30);
[X1,Y1] = meshgrid(xlin,ylin);
W1     = griddata(X,Y,Z1,X1,Y1,'cubic');
trimesh(t(1:3,:).',X,Y,Z1,'edgecolor','y'), hold on
for I = 1:size(e,2)
   plot(p(1,e(1:2,I)),p(2,e(1:2,I)),'r','linewidth',2), hold on
end
axis equal, axis manual
switch bilda
case 1,
   disp(' exakt Contour    for pressure (red)')
   disp(' numerical Contour for pressure (black) ')
   W = griddata(X,Y,P,X1,Y1,'cubic');
   [C,h] = contour(X1,Y1,W,[0.1,0.2,0.4,0.6,0.8,1],'k');
   %[C,h] = contour(X1,Y1,W,10);
   % clabel(C,h);
   W = griddata(X,Y,PE,X1,Y1,'cubic');
   [C,h] = contour(X1,Y1,W,[0.1,0.2,0.4,0.6,0.8,1],'r');
  % [C,h] = contour(X1,Y1,W,10);
  % clabel(C,h);
  %WERT_N = PE(size(p,2))
  % P_WERT= [RDP(1),RDP(2)]
case 2, disp(' streamslice for stream function ')
   U1 = griddata(X,Y,U,X1,Y1,'v4');
   V1 = griddata(X,Y,V,X1,Y1,'v4');
   streamslice(X1,Y1,U1,V1)
case 3, disp(' streamslice for exact stream function ')
   U1 = griddata(X,Y,UE,X1,Y1,'v4');
   V1 = griddata(X,Y,VE,X1,Y1,'v4');
   streamslice(X1,Y1,U1,V1)
end
%clear
