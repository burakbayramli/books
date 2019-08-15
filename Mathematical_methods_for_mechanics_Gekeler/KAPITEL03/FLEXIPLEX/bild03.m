function bild03
% Beispiel Himmelblau, S. 359/360
clf,
plot(0,0,'w.'), hold on
plot(6,6,'w.'), hold on
axis equal tight, axis manual, grid on
%load daten PFAD
%plot(PFAD(:,1),PFAD(:,2),'r','linewidth',2), hold on
%plot(PFAD(:,1),PFAD(:,2),'r.','markersize',12), hold on
X = linspace(0,6,60); Y = linspace(0,6,60);
[U,V] = meshgrid(X,Y);
m = length(X); Z1 = zeros(m,m); Z2 = Z1; Z3 = Z1;
for i = 1:m
   for k = 1:m
      XX = [X(i),Y(k)];
      Z1(k,i) = feval(@bsp03,XX,1);
      Z2(k,i) = feval(@bsp03,XX,3);
      AUX = feval(@bsp03,XX,2);
      Z3(k,i) = AUX(1);
   end
end
W1    = griddata(X,Y,Z1,U,V);
W2    = griddata(X,Y,Z2,U,V);
W3    = griddata(X,Y,Z3,U,V);
contour(U,V,W1,[-32 -20 -10 0],'b'), hold on
contour(U,V,W2,[0 0],'r'), hold on
contour(U,V,W3,[0 0],'g'), hold on
