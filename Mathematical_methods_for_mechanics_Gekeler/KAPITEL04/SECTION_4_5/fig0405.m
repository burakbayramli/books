function fig0405
% Figure 4.5, Example 4.9

disp(' Call first DEMO1-1 ! ')
clf
load daten01a X Parmeter
n = Parmeter(1); RAD = 0.02;
X1 = X(1:n+1); X2 = X(n+2:2*(n+1)); U = X(4*n+5:5*(n+1));
plot(X1,X2,'k','Linewidth',2), hold on
plot(X1,X2,'k.','Markersize',6),hold on
A = [X1(1), X2(1)]; B1 = [X1(n+1),X2(n+1)];
VEC =  [cos(U), sin(U)];
quiver(X1,X2,VEC(:,1),VEC(:,2),0.3), hold on
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
load daten01b X Parmeter
n = Parmeter(1);
X1 = X(1:n+1); X2 = X(n+2:2*(n+1)); U = X(4*n+5:5*(n+1));
plot(X1,X2,'k','Linewidth',2), hold on
plot(X1,X2,'k.','Markersize',6), hold on
B2 = [X1(n+1),X2(n+1)];
VEC =  [cos(U), sin(U)];
quiver(X1,X2,VEC(:,1),VEC(:,2),0.3), hold on
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
load daten01c X Parmeter
n = Parmeter(1);
X1 = X(1:n+1); X2 = X(n+2:2*(n+1)); U = X(4*n+5:5*(n+1));
plot(X1,X2,'k','Linewidth',2), hold on
plot(X1,X2,'k.','Markersize',6), hold on
B3 = [X1(n+1),X2(n+1)];
VEC =  [cos(U), sin(U)];
quiver(X1,X2,VEC(:,1),VEC(:,2),0.3), hold on
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
load daten01d X Parmeter
n = Parmeter(1);
X1 = X(1:n+1); X2 = X(n+2:2*(n+1)); U = X(4*n+5:5*(n+1));
plot(X1,X2,'k','Linewidth',2), hold on
plot(X1,X2,'k.','Markersize',6), hold on
B4 = [X1(n+1),X2(n+1)];
VEC =  [cos(U), sin(U)];
quiver(X1,X2,VEC(:,1),VEC(:,2),0.3), hold on
circle(A(1),A(2),RAD,'w')
circle(B1(1),B1(2),RAD,'w')
circle(B2(1),B2(2),RAD,'w')
circle(B3(1),B3(2),RAD,'w')
circle(B4(1),B4(2),RAD,'w')
axis equal, grid off
