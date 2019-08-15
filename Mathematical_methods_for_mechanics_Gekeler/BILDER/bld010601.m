% BILD006
clf
[X,Y] = meshgrid(-1:0.2:2,-1:0.2:2);
U = Y.*(1 - X);
V = X.*(Y-1);
streamslice(X,Y,U,V)
hold on
% Achsen ---------------------
X = [-1,2];
Y = [0,0];
c = 0.1;
d = 0.05;
arrow(X,Y,c,d,'k',2)
hold on
X = [0,0];
Y = [-1,2];
c = 0.1;
d = 0.05;
arrow(X,Y,c,d,'k',2)
hold on
% Singulaere Punkte ----------------
X = 0;
Y = 0;
R = 0.02;
circle(X,Y,R,'w')
%plot(X,Y,'o','Markersize',6)
hold on
X = 1;
Y = 1;
plot(X,Y,'o','Markersize',6)
hold on
% Separatrizen --------------
d = 0.05;
X = [-1,1-d];
Y = [1,1];
plot(X,Y,'k','linewidth',2)
hold on
X = [1+d,2];
Y = [1,1];
plot(X,Y,'k','linewidth',2)
hold on
X = [1,1];
Y = [-1,1-d];
plot(X,Y,'k','linewidth',2)
hold on
X = [1,1];
Y = [1+d,2];
plot(X,Y,'k','linewidth',2)
axis equal tight
