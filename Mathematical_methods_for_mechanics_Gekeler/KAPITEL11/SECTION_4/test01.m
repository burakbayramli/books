% Testen Splines von H.
clf

P = [0, 0.5, 1, 1, 0;
     0, 0.3, 0, 1, 1]';
M = size(P,1);
P2 = [P(M-2:M,:);P];
knots=[-3:(M+3)];
spl.parameter= 1;
spl.dimension=2;
spl.degree=3;
spl.points=P2;
spl.knots=knots;
%e=spl_eval(spl,linspace(0,4,50));
e=spl_eval(spl,linspace(0,M,50));
plot(e(:,1),e(:,2))
