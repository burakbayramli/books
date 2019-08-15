function p1 = mesh02(p,e,t);
% Eckart Gekeler, Universitaet Stuttgart, Release 8.4.05
% verschiebt Knoten manuell
% fuer Dreiecke und Vierecke
disp('Zum Beenden auf Rahmens tippen')
xmin = min(p(1,:));
xmax = max(p(1,:));
ymin = min(p(2,:));
ymax = max(p(2,:));
axis([xmin xmax ymin ymax]);
grid on, hold on
p1 = p;
while (1)
   [U,V]     = ginput(1);
   if U < xmin | U > xmax, break, end
   if V < ymin | V > ymax, break, end
   DIST      = p1(1:2,:);
   DIST(1,:) = DIST(1,:) - U;
   DIST(2,:) = DIST(2,:) - V;
   i         = sqrt(-1);
   DIST      = abs(DIST(1,:) + i*DIST(2,:));
   AUX       = min(DIST);
   J         = find(DIST == AUX);
   p1(1:2,J)  = [U; V];
   X         = p1(1,:);
   Y         = p1(2,:);
   Z         = zeros(1,size(p1,2));
   clf
   axis([xmin xmax ymin ymax]);
   grid on, hold on
   trimesh(t',X,Y,Z);
   hold on
   plot(X,Y,'.','MarkerSize',6);
end
grid off
