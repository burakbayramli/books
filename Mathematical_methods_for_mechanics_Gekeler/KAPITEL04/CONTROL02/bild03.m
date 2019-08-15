function bild03
% Figure for Ex. 3, Zermelo's problem
load daten3 X Parmeter2
n = Parmeter2(2);
clf
U = [cos(X(4,:)); sin(X(4,:))];
A = [X(1,1), X(1,n+1); X(2,1), X(2,n+1)];
plot(X(1,:),X(2,:),'r'),hold on
plot(X(1,:),X(2,:),'.','Markersize',6), hold on
plot(A(1,:),A(2,:),'.','Markersize',12), hold on
quiver(X(1,:),X(2,:),U(1,:),U(2,:),0.2);
axis([-1 6 -2 1])
axis equal, grid on
title('Example 3','fontsize',18)
