% Testen Splines von H.
P = [0, 1, 1, 0; 0, 0, 1, 1]';
P2 = [P(2:4,:);P];
knots=[-3:6];
spl.parameter= 1;
spl.dimension=2;
spl.degree=3;
spl.points=P2;
spl.knots=knots;
e=spl_eval(spl,linspace(0,4,50));
plot(e(:,1),e(:,2))

