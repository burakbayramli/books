function Y = bezier(X)
% Zeichnet geschlossene Bezierkurve
Y = X;
NN = 100; % Anzahl der Punkte
Y = Y';
M = size(Y,1);
P2 = [Y(M-2:M,:);Y];
knots=[-3:(M+3)];
spl.parameter= 1;
spl.dimension=2;
spl.degree=3;
spl.points=P2;
spl.knots=knots;
e=spl_eval(spl,linspace(0,M,NN));
plot(e(:,1),e(:,2))
Y = e';
