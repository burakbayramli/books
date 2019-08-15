% Abb. 74
clf
N     = 30;
A     = 0.75;
X     = linspace(-A,A,N);
Y     = linspace(-A,A,N);
[X,Y] = meshgrid(X,Y);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
subplot(1,3,1)
mu    = - 0.5;
U     = (mu*X - Y);
V     = (X  + mu*Y);
streamslice(X,Y,U,V)
axis equal tight
title('\mu = - 0.5', 'Fontsize',12)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
subplot(1,3,2)
mu    = 0;
U     = (mu*X - Y);
V     = (X  + mu*Y);
streamslice(X,Y,U,V)
axis equal tight
title('\mu = 0', 'Fontsize',12)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
subplot(1,3,3)
mu    = 0.5;
U     = (mu*X - Y);
V     = (X  + mu*Y);
streamslice(X,Y,U,V)
axis equal tight
title('\mu = 0.5', 'Fontsize',12)
