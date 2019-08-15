% Abb. 76
clf
N     = 30;
X     = linspace(-1,1,N);
Y     = linspace(-1,1,N);
[X,Y] = meshgrid(X,Y);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
subplot(1,4,4)
mu    = 0.5;
SQRT  = sqrt(X.*X + Y.*Y);
U     = (mu *X - Y) + SQRT.*X;
V     = (X  + mu*Y) + SQRT.*Y;
streamslice(X,Y,U,V)
axis equal tight
title('\mu = 0.5', 'Fontsize',12)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
subplot(1,4,3)
mu    = 0;
U     = (mu*X - Y) + SQRT.*X;;
V     = (X + mu*Y) + SQRT.*Y;
streamslice(X,Y,U,V)
axis equal tight
title('\mu = 0', 'Fontsize',12)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
subplot(1,4,2)
mu  = - 0.5;
U     = (mu*X - Y) + SQRT.*X;
V     = (X + mu*Y) + SQRT.*Y;
streamslice(X,Y,U,V), hold on
R = mu;
TT = linspace(0,2*pi,40);
X1 = R*cos(TT); Y1 =  R*sin(TT);
plot(X1,Y1,'k','linewidth',2)
axis equal tight
title('\mu = - 0.5', 'Fontsize',12)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
subplot(1,4,1)
mu  = - 1;
U     = (mu*X - Y) + SQRT.*X;
V     = (X + mu*Y) + SQRT.*Y;
streamslice(X,Y,U,V), hold on
R = mu;
TT = linspace(0,2*pi,40);
X1 = R*cos(TT); Y1 =  R*sin(TT);
plot(X1,Y1,'k','linewidth',2)
axis equal tight
title('\mu = - 1', 'Fontsize',12)

