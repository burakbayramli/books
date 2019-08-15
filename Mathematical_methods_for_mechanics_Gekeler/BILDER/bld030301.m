function bld030301
% BILD042
clf
% Beispiel (l) zur Linearen Optimierung
X  = [ 0 6 6 4 2 0 0;
       0 0 1 4 5 5 0];
X1 = [ 6 6 4 2 0 ;
       0 1 4 5 5 ];
X2 = [ 8 2;
       1 7];
X3 = [ 6 6 4 2 0 5;
       0 1 4 5 5 4];
U1 = [ 0 1 3 1  0;
      -1 0 2 2  1];
U2 = [ 1 3 1 0 -1;
       0 2 2 1  0];
U3 = [ 1 1 1 1  1 1;
       1 1 1 1  1 1];
plot(X(1,:),X(2,:),'b'),hold on
fill(X(1,:),X(2,:),'y'),hold on
plot(X(1,:),X(2,:),'.','Markersize',6), hold on
plot(X2(1,:),X2(2,:),'k','linewidth',2), hold on
c = 0.25; d = 0.06;
Y1 = X1 + U1;
myquiver(X1,Y1,c,d,'r',1,1);
hold on
Y2 = X1 + U2;
myquiver(X1,Y2,c,d,'r',1,1);
hold on
Y3 = X3 + U3;
myquiver(X3,Y3,c,d,'k',1,1);
hold on
plot(9,7,'w')
text(2.5,2.5,'S','fontsize',36)
text(6.3,3.3,'f','fontsize',22)
%axis([0 9 0 7])
axis equal tight
grid off
