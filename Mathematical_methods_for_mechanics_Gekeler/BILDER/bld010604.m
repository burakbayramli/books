% BILD016
clf
X0 = - 5;
X1 = 2;
X3 = [X0,X1];
Y3 = [X0+2,X1+2];
Y4 = [X0+4,X1+4];
plot(X3,Y3,X3,Y4,'k','linewidth',2)
hold on
X = linspace(-5,2,50);
Y = linspace(-3,5,50);
[X,Y] = meshgrid(X,Y);
[m,n] = size(X);
U = ones(m,n);
V = (X - Y + 3).^2;
streamslice(X,Y,U,V)
