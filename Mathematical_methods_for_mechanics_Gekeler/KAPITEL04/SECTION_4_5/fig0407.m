function fig0407
% Figure 4.7, Ex. 4.11, several paths

disp(' Call first DEMO1-3 ! ')

clf
load daten03a X Parmeter
n = Parmeter(1); X1 = X(1:n+1); X2 = X(n+2:2*(n+1));
U   = X(2*n+3:3*(n+1));
T1   =  X(3*n+4)
VEC = [cos(U), sin(U)];
A   = [X1(1), X1(n+1); X2(1), X2(n+1)];
plot(X1,X2,'r'), hold on
plot(X1,X2,'.','Markersize',6), hold on
plot(A(1,:),A(2,:),'.','Markersize',12), hold on
quiver(X1,X2,VEC(:,1),VEC(:,2),0.2), hold on
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
load daten03b X Parmeter
n   = Parmeter(1);
X1  = X(1:n+1); X2  = X(n+2:2*(n+1));
U   = X(2*n+3:3*(n+1));
T2   =  X(3*n+4)
VEC = [cos(U), sin(U)];
A   = [X1(1), X1(n+1); X2(1), X2(n+1)];
plot(X1,X2,'r'), hold on
plot(X1,X2,'.','Markersize',6), hold on
plot(A(1,:),A(2,:),'.','Markersize',12), hold on
quiver(X1,X2,VEC(:,1),VEC(:,2),0.3), hold on
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
load daten03c X Parmeter
n   = Parmeter(1);
X1  = X(1:n+1); X2  = X(n+2:2*(n+1));
U   = X(2*n+3:3*(n+1));
T3   =  X(3*n+4)
VEC = [cos(U), sin(U)];
A   = [X1(1), X1(n+1); X2(1), X2(n+1)];
plot(X1,X2,'r'), hold on
plot(X1,X2,'.','Markersize',6), hold on
plot(A(1,:),A(2,:),'.','Markersize',12), hold on
quiver(X1,X2,VEC(:,1),VEC(:,2),0.3), hold on
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
load daten03d X Parmeter
n   = Parmeter(1);
X1  = X(1:n+1); X2  = X(n+2:2*(n+1));
U   = X(2*n+3:3*(n+1));
T4   =  X(3*n+4)
VEC = [cos(U), sin(U)];
A   = [X1(1), X1(n+1); X2(1), X2(n+1)];
plot(X1,X2,'r'), hold on
plot(X1,X2,'.','Markersize',6), hold on
plot(A(1,:),A(2,:),'.','Markersize',12), hold on
quiver(X1,X2,VEC(:,1),VEC(:,2),0.3), hold on
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
load daten03e X Parmeter
n   = Parmeter(1);
X1  = X(1:n+1); X2  = X(n+2:2*(n+1));
U   = X(2*n+3:3*(n+1));
T5   =  X(3*n+4)
VEC = [cos(U), sin(U)];
A   = [X1(1), X1(n+1); X2(1), X2(n+1)];
plot(X1,X2,'r'), hold on
plot(X1,X2,'.','Markersize',6), hold on
plot(A(1,:),A(2,:),'.','Markersize',12), hold on
quiver(X1,X2,VEC(:,1),VEC(:,2),0.3), hold on
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
load daten03f X Parmeter
n   = Parmeter(1);
X1  = X(1:n+1); X2  = X(n+2:2*(n+1));
U   = X(2*n+3:3*(n+1));
T6   =  X(3*n+4)
VEC = [cos(U), sin(U)];
A   = [X1(1), X1(n+1); X2(1), X2(n+1)];
plot(X1,X2,'g'), hold on
plot(X1,X2,'.','Markersize',6), hold on
plot(A(1,:),A(2,:),'.','Markersize',12), hold on
quiver(X1,X2,VEC(:,1),VEC(:,2),0.3), hold on

axis([-0.5 7.5 -3 2])
axis equal, grid off
%FAHRTDAUER = T
