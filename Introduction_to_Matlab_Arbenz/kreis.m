% Kreisanpassung nach der Methode der kleinsten Quadrate
% unter Verwendung des Gauss-Newton Verfahrens
% (zeigt auch einige Moeglichkeiten von MATLAB)
%
xi = [ 0.7 3.3 5.6 7.5 6.4 4.4 0.3 -1.1]'; % Gegebene
% Messpunkte
eta = [ 4.0 4.7 4.0 1.3 -1.1 -3.0 -2.5 1.3]';
%
[xi, eta]
xmin = min(xi); xmax = max(xi); ymin = min(eta); ymax = max(eta);
dx = (xmax - xmin)/10; dy = (ymax -ymin)/10;
axis([xmin-dx xmax+dx ymin-dy ymax+dy]);
axis('equal');
% damit Einheiten auf beiden Achsen etwa
% gleich sind
hold;
% nachfolgende Plots im gleichen
% Koordinatensystem

plot(xi,eta,'o'); 
phi = [0:0.02:2*pi];
h = size(xi); n = h(1);
x = [0.1 1 1]';
format long; minimum = [];

while norm(h)>norm(x)*1e-4,
  u = x(1) + x(3)*cos(phi);
  v = x(2) + x(3)*sin(phi);
  i = i-0.05;
  plot(u,v);
  a = x(1)-xi; b = x(2)-eta;
  fak = sqrt(a.*a + b.*b);
  J = [a./fak b./fak -ones(size(a))];
  f = fak -x(3);
  h = -J\f;
  x = x + h;
  [x h ], pause
  minimum = [minimum norm(f)];
end;
minimum'

