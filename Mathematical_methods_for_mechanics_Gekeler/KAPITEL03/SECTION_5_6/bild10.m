function bild04
clf
plot(5,2.5,'k','markersize',6), hold on
Parmeter = [];
X0 = [0;0];
Z = bsp04(X0,7,Parmeter);
W = [[Z(1,:),Z(3,:),Z(5,:)];[Z(2,:),Z(4,:),Z(6,:)]];
fill(W(1,:),W(2,:),'y'), hold on
plot(Z(1,:),Z(2,:),'k'), hold on
plot(Z(3,:),Z(4,:),'b'), hold on
plot(Z(5,:),Z(6,:),'r'), hold on
XA = linspace(0,5,60); Y = linspace(-1,2.5,60);
[U,V] = meshgrid(XA,Y);
m  = length(XA); Z1 = zeros(m,m); Z2 = zeros(m,m); Z3 = zeros(m,m);
% Zielfunktion ------------------------
for i = 1:m
   for k = 1:m
      U1 = [XA(i);Y(k)];
      Z1(k,i) = feval('bsp04',U1,1,Parmeter);
   end
end
X_OPT1 = [2.43;1.27];
F_OPT = feval('bsp04',X_OPT1,1,Parmeter);
W1    = griddata(XA,Y,Z1,U,V);
contour(U,V,W1,[5 7 9 15 F_OPT],'k'), hold on
plot(X_OPT1(1),X_OPT1(2),'.','Markersize',6), hold on
