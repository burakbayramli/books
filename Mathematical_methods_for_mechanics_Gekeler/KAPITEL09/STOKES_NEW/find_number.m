function N = find_number(p,t)
% Eckart Gekeler, Universitaet Stuttgart, Release 8.4.05
% Finds NUmber of a point
% fuer Dreiecke und Vierecke
disp('Zum Beenden auf Rahmens tippen')
xmin = min(p(1,:));
xmax = max(p(1,:));
ymin = min(p(2,:));
ymax = max(p(2,:));
clf,
axis([xmin xmax ymin ymax]);

grid on, hold on
X         = p(1,:);
Y         = p(2,:);
Z         = zeros(1,size(p,2));
trimesh(t(1:3,:).',X,Y,Z), hold on

while (1)
   [U,V]     = ginput(1);
   if U < xmin | U > xmax, break, end
   if V < ymin | V > ymax, break, end
   DIST      = p(1:2,:);
   DIST(1,:) = DIST(1,:) - U;
   DIST(2,:) = DIST(2,:) - V;
   i         = sqrt(-1);
   DIST      = abs(DIST(1,:) + i*DIST(2,:));
   AUX       = min(DIST);
   N         = find(DIST == AUX)
   
   X         = p(1,:);
   Y         = p(2,:);
   Z         = zeros(1,size(p,2));
   plot(p(1,N),p(2,N),'k*','MarkerSize',6);
end
grid off
