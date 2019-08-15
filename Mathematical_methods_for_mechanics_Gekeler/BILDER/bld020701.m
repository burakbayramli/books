% Van der Pol gleichung vgl. Hairer II, S. 400 ff.
clf
epslon = 0.05;
epsinv = 1/epslon;
X = linspace(-4,4,400);
Y = linspace(-6,6,400);
[X,Y] = meshgrid(X,Y);
[m,n] = size(X);
U = Y;
V = epsinv*((1 - X.*X).*Y - X);
streamslice(X,Y,U,V), hold on
XX = linspace(-0.92,0.92,100);
YY = XX./(1 - XX.*XX);
plot(XX,YY,'k','linewidth',2), hold on
XX = linspace(-3.4,-1.09,100);
YY = XX./(1 - XX.*XX);
plot(XX,YY,'k','linewidth',2), hold on
XX = linspace(1.09,3.4,100);
YY = XX./(1 - XX.*XX);
plot(XX,YY,'k','linewidth',2), hold on

axis equal tight
