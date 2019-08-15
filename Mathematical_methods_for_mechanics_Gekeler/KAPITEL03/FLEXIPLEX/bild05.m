function bild05
clf
plot(-1.5,-2,'k','markersize',6), hold on
plot(1.5,2,'k','markersize',6), hold on

Parmeter = []; X0 = [0;0];
Z = bsp05(X0,4,Parmeter);
W = [[Z(1,:), fliplr(Z(1,:))]; [Z(2,:), fliplr(Z(3,:))]];
fill(W(1,:),W(2,:),'y'), hold on
X = linspace(-1.5,1.5,60); Y = linspace(-2.5,2.5,60);
[U,V] = meshgrid(X,Y);
m = length(X); Z1 = zeros(m,m); Z2 = zeros(m,m); Z3 = zeros(m,m);
% Zielfunktion ------------------------
for i = 1:m
   for k = 1:m
      U1 = [X(i);Y(k)];
      Z1(k,i) = feval(@bsp05,U1,1,Parmeter);
   end
end
X_OPT = [0.546;0.702];
F_OPT = feval(@bsp05,X_OPT,1,Parmeter);
W1    = griddata(X,Y,Z1,U,V);
%[C,h] = contour(U,V,W1,[F_OPT F_OPT],'k');
%clabel(C,h,'manual');
hold on
contour(U,V,W1,[0.2 0.6, F_OPT],'k'), hold on
plot(X_OPT(1),X_OPT(2),'.','Markersize',6), hold on
