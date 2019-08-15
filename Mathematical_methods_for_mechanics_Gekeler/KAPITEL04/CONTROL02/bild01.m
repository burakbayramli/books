function bild01
% Figure for Ex. 1, Thrust problem
load daten1 X n
clf
TANGENS = X(8,:);
V = atan(TANGENS);
U = [cos(V); sin(V)];
A = [X(1,1), X(1,n+1); X(2,1), X(2,n+1)];
plot(X(1,:),X(2,:),'r'), hold on
plot(X(1,:),X(2,:),'.','Markersize',6), hold on
plot(A(1,:),A(2,:),'.','Markersize',12), hold on
quiver(X(1,:),X(2,:),U(1,:),U(2,:),0.2);
%axis([0 1 0 1])
axis equal, grid on
title('Example 1','fontsize',18)
