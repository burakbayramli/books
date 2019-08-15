function bild04a
% Figure to Example 4, Nominal control

load daten4 X1 Y1 U1
clf
plot(X1(1,:),X1(2,:),'k'), hold on
quiver(X1(1,:),X1(2,:),cos(U1),sin(U1),0.2);
axis equal, grid on
