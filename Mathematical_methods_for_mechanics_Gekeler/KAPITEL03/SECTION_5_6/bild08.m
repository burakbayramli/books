function bild02
clf
Parmeter = [];
plot(1.5,2,'k','markersize',6), hold on
X0 = [0;0];
Z = bsp02(X0,7,Parmeter);
W = [[Z(1,:), fliplr(Z(1,:))]; [Z(2,:), fliplr(Z(3,:))]];
fill(W(1,:),W(2,:),'y'), hold on
XZ = linspace(-1,1.5,60); YZ = linspace(-1,2,60);
[U,V] = meshgrid(XZ,YZ);
m = length(XZ); Z1 = zeros(m,m); Z2 = zeros(m,m); Z3 = zeros(m,m);
% Zielfunktion ------------------------
for i = 1:m
   for k = 1:m
      U1 = [XZ(i);YZ(k)];
      Z1(k,i) = feval('bsp02',U1,1,Parmeter);
   end
end
X_OPT = [0.546;0.702];
F_OPT = feval('bsp02',X_OPT,1,Parmeter);
W1    = griddata(XZ,YZ,Z1,U,V);
%[C,h] = contour(U,V,W1,[F_OPT F_OPT],'k');
%clabel(C,h,'manual');
%contour(U,V,W1)
hold on
contour(U,V,W1,[0.2 0.6],'k'), hold on
contour(U,V,W1,[F_OPT F_OPT],'k'), hold on
plot(X_OPT(1),X_OPT(2),'*','Markersize',12), hold on
