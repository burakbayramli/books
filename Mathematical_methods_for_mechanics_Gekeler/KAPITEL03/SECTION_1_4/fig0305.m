function fig0305
% Figure 3.5, BFGS und Steepest descend

clc
disp(' Call first DEMO1.M ')
load daten1 G1
load daten2 G2
clf, hold on
X = linspace(-5,25,45); Y = linspace(-5,25,45);
m = length(X); Z = zeros(m,m);
for i = 1:m
   for k = 1:m
      U = [X(i);Y(k)];
      Z(k,i) = feval('bsp01',U,1);
   end
end
[U,V] = meshgrid(X,Y);
W     = griddata(X,Y,Z,U,V);
contour(U,V,W,[70,50,30,10,-10,-19,-29,-39,-49,-60,-63 ],'color','k'),hold on
A   = [15.37622;13.78569];
MIN = feval('bsp01',A,1)
plot(A(1),A(2),'+','Markersize',6), hold on
plot(G1(1,:),G1(2,:),'k.-','linewidth',2), hold on
plot(G2(1,:),G2(2,:),'r*:','linewidth',2), hold on
grid off






