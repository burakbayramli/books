function bild04b
% Figure to Example 4, Dyer-McReynolds, p. 66

load daten5 X2 Y2 U2
clf
plot(X2(1,:),X2(2,:),'k'), hold on
quiver(X2(1,:),X2(2,:),cos(U2),sin(U2),0.2);
axis equal, grid on
%clear
