function test01
% Figure 5.8, individual representations

clc, clf
N     = 50;
X     = linspace(-1,1,N);
Y     = linspace(-1,1,N);
[X,Y] = meshgrid(X,Y);
SQRT  = sqrt(X.*X + Y.*Y);

nr = 100; KK = [1,2,3,4,5];
while ~ismember(nr,KK)
   nr   = input(' Example no. (1/2/3)? ');
end;
switch nr
case 1
   mu = 0.5;
   U  = (mu *X - Y) - SQRT.*X;
   V  = (X  + mu*Y) - SQRT.*Y;
   streamslice(X,Y,U,V), hold on
   R = mu;
   TT = linspace(0,2*pi,40);
   X1 = R*cos(TT); Y1 =  R*sin(TT);
   plot(X1,Y1,'k','linewidth',2)
   axis equal tight
   title('\mu = 0.5', 'Fontsize',12)
case 2
   mu = -0.5;
   U  = (mu*X - Y) + SQRT.*X;
   V  = (X + mu*Y) + SQRT.*Y;
   streamslice(X,Y,U,V),hold on
   R = abs(mu);
   TT = linspace(0,2*pi,40);
   X1 = R*cos(TT); Y1 =  R*sin(TT);
   plot(X1,Y1,'k','linewidth',2)
   axis equal tight
case 3
   X = linspace(-0.3,0.3,N); Y = linspace(-0.3,0.3,N);
   [X,Y] = meshgrid(X,Y);
   mu  = 0.02;
   mu = - 0.04;
   U = (mu*X - Y) + (X.*X + Y.*Y).*X;
   V = (X + mu*Y) + (X.*X + Y.*Y).*Y;
   streamslice(X,Y,U,V), hold on
   R = sqrt(abs(mu));
   TT = linspace(0,2*pi,40);
   X1 = R*cos(TT); Y1 =  R*sin(TT);
   plot(X1,Y1,'k','linewidth',2)
   axis equal tight
   %title('\mu = - 1', 'Fontsize',12)
end
