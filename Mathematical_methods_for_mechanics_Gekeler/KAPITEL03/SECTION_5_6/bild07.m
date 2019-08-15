function bild01
clf
Parmeter = []; X0 = [0;0];
plot(1.5,3,'k','markersize',6), hold on
Z = bsp01(X0,7,Parmeter);
W = [[Z(1,:), fliplr(Z(1,:))]; [Z(2,:), fliplr(Z(3,:))]];
fill(W(1,:),W(2,:),'y'), hold on
X = linspace(-1.5,1.5,60); Y = linspace(-3,3,60);
[U,V] = meshgrid(X,Y);
m = length(X); Z1 = zeros(m,m); Z2 = zeros(m,m); Z3 = zeros(m,m);
% Zielfunktion ------------------------
for i = 1:m
   for k = 1:m
      U1 = [X(i);Y(k)];
      Z1(k,i) = feval('bsp01',U1,1,Parmeter);
   end
end
X_OPT = [-1;0];
F_OPT = feval('bsp01',X_OPT,1,Parmeter);
W1    = griddata(X,Y,Z1,U,V);
%[C,h] = contour(U,V,W1,[F_OPT F_OPT],'k');
%clabel(C,h,'manual');
hold on
contour(U,V,W1,[3 4],'k'), hold on
contour(U,V,W1,[F_OPT F_OPT],'k'), hold on
contour(U,V,W1,[6 6],'k'), hold on
plot(X_OPT(1),X_OPT(2),'.','Markersize',6), hold on
