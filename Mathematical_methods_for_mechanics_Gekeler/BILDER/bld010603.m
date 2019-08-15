% BILD008
clf
X =[-5,5];
Y = [0,0];
arrow(X,Y,0.2,0.1,'k',2)
hold on

X =[0,0];
Y = [-3,7];
arrow(X,Y,0.2,0.1,'k',2)
hold on

X1 = 0;
Y1 = - log(2);
plot(X1,Y1,'.','Markersize',6)
hold on

X = linspace(-5,5,50);
Y = linspace(-3,7,50);
[X,Y] = meshgrid(X,Y);
[m,n] = size(X);
U = ones(m,n);
V = exp(Y).*sin(X);
streamslice(X,Y,U,V)
hold on
X1 = -5;
Y1 = - log(2) + 0.35;
h = streamline(X,Y,U,V,X1,Y1);
set(h,'linewidth',2,'color','k')
