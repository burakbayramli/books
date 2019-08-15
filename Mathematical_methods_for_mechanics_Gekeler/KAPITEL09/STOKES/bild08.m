function bild08
% Streamslice for (U,V), streamlines, CONTOUR for P, QUIVER for (U,V),
bilda = 100;
while ~ismember(bilda,[1,2,3,4])
   bilda = input(' Select figure! (1/2/3/4) ');
end
%bilda = 3;
load daten14a p e t p1 t1
load daten14b U V P
X = p(1,:); Y = p(2,:); Z1 = zeros(1,length(X)); P = P';
clf, hold on
N1 = size(p,2);
U = U(1:N1); V = V(1:N1);
xlin  = linspace(min(X),max(X),30);
ylin  = linspace(min(Y),max(Y),30);
[X1,Y1] = meshgrid(xlin,ylin);
W1     = griddata(X,Y,Z1,X1,Y1,'cubic');
trimesh(t(1:3,:).',X,Y,Z1,'edgecolor','g'), hold on
for I = 1:size(e,2)
   plot(p(1,e(1:2,I)),p(2,e(1:2,I)),'r','linewidth',2), hold on
end
LU = [min(X),min(Y)]; LU = LU - 1;
RO = [max(X),max(Y)]; RO = RO + 1;
plot(LU(1),LU(2),'w.'), hold on
plot(RO(1),RO(2),'w.'), hold on

axis equal tight, axis manual, grid off
switch bilda
case 1, disp(' Streamslice  ')
   U1 = griddata(X,Y,U,X1,Y1,'cubic');
   V1 = griddata(X,Y,V,X1,Y1,'cubic');
   streamslice(X1,Y1,U1,V1)
   weisseln(p,e,t)
case 2, disp(' Streamlines ')
   U1 = griddata(X,Y,U,X1,Y1,'cubic');
   V1 = griddata(X,Y,V,X1,Y1,'cubic');
   %streamslice(X1,Y1,U1,V1)
   A = [0,0.25,0.5,1,1.5,2,2.5,2.75];
   B = zeros(1,length(A));
   H = streamline(X1,Y1,U1,V1,A,B);
   set(H,'linewidth',2)
case 3, disp(' Contour for P ')
   W = griddata(X,Y,P,X1,Y1,'cubic');
   contour(X1,Y1,W,10);
  % [C,h] = contour(X1,Y1,W,10);
  % clabel(C,h,'manual');
case 4, disp(' flow ')
    U = U.'; V = V.';
    quiver(X,Y,U,V,2), hold on
end
axis off
clear
