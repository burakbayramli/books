% BILD007
clf
% QUIVER -----------------------
[X,Y] = meshgrid(-2.5:0.1:2.5,-2.5:0.1:2.5);
U = Y.*(1 - X.*X);
V = X.*(Y.*Y - 1);
streamslice(X,Y,U,V)
hold on
% STRECKEN ----------------------
d = 0.03;
X = [-1+d,1-d];
Y = [-1,-1];
plot(X,Y,'LineWidth',2);
hold on
X = [1,1];
Y = [-1+d,1-d];
plot(X,Y,'LineWidth',2);
hold on
X = [-1+d,1-d];
Y = [1,1];
plot(X,Y,'LineWidth',2);
hold on
X = [-1,-1];
Y = [-1+d,1-d];
plot(X,Y,'LineWidth',2);
hold on
% HALBGERADEN ---------------------
X = [1+d,2.5];
Y = [1,1];
plot(X,Y,'LineWidth',2);
hold on
X = [1,1];
Y = [1+d,2.5];
plot(X,Y,'LineWidth',2);
hold on
X = [-1,-1];
Y = [1+d,2.5];
plot(X,Y,'LineWidth',2);
hold on
X = [-1-d,-2.5];
Y = [1,1];
plot(X,Y,'LineWidth',2);
hold on
X = [-1-d,-2.5];
Y = [-1,-1];
plot(X,Y,'LineWidth',2);
hold on
X = [-1,-1];
Y = [-1-d,-2.5];
plot(X,Y,'LineWidth',2);
hold on
X = [1,1];
Y = [-1-d,-2.5];
plot(X,Y,'LineWidth',2);
hold on
X = [1+d,2.5];
Y = [-1,-1];
plot(X,Y,'LineWidth',2);
hold on
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
e = 0.25; f = 0.1;
%arrow_1(e,f)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
X0 = [0, 0];
plot(X0(1),X0(2),'o','MarkerSize',6);
hold on
X1 = [1,1];
plot(X1(1),X1(2),'o','MarkerSize',6);
hold on
X2 = [-1,1];
plot(X2(1),X2(2),'o','MarkerSize',6);
hold on
X3 = [-1,-1];
plot(X3(1),X3(2),'o','MarkerSize',6);
hold on
X4 = [1,-1];
plot(X4(1),X4(2),'o','MarkerSize',6);
hold on

c = 0.3;
d = 0.1;

X = [-1.8,-2];
Y = [1,1];
arrow_2(c,d,X,Y)
hold on

X = [0,0.3];
Y = [1,1];
arrow_2(c,d,X,Y)
hold on

X = [2,1.8];
Y = [1,1];
arrow_2(c,d,X,Y)
hold on

X = [-1.9,-1.7];
Y = [-1,-1];
arrow_2(c,d,X,Y)
hold on

X = [0,-0.3];
Y = [-1,-1];
arrow_2(c,d,X,Y)
hold on

X = [1.8,2];
Y = [-1,-1];
arrow_2(c,d,X,Y)
hold on

X = [-1,-1];
Y = [2,1.8];
arrow_2(c,d,X,Y)
hold on

X = [-1,-1];
Y = [-0.1,0.2];
arrow_2(c,d,X,Y)
hold on

X = [-1,-1];
Y = [-1.5,-1.8];
arrow_2(c,d,X,Y)
hold on

X = [1,1];
Y = [1.8,2];
arrow_2(c,d,X,Y)
hold on

X = [1,1];
Y = [0.2,-0.1];
arrow_2(c,d,X,Y)
hold on

X = [1,1];
Y = [-1.8,-1.5];
arrow_2(c,d,X,Y)
hold on

axis([-2.5 2.5 -2.5 2.5])
axis square
