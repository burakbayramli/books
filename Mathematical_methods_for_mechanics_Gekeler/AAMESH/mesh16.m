function KNOTEN = mesh16(KNOTEN,ELEMENTE,RANDDAT);
% Eckart Gekeler, Universitaet Stuttgart, Release 8.4.05
% fuer Dreiecke verschiebt Knoten manuell

clf
xmin = min(KNOTEN(:,2));
xmax = max(KNOTEN(:,2));
ymin = min(KNOTEN(:,3));
ymax = max(KNOTEN(:,3));
axis([xmin xmax ymin ymax]);
grid on
hold on
X         = KNOTEN(:,2);
Y         = KNOTEN(:,3);
M         = size(KNOTEN,1);
Z         = zeros(M,1);
plot(X,Y,'.','MarkerSize',6);
hold on;
trimesh(ELEMENTE,X,Y,Z);
hold on
plot(X,Y,'.','MarkerSize',6);
if isempty(RANDDAT) == 0
   plot(RANDDAT(:,1),RANDDAT(:,2),'r');
   hold on
end
while (1)
   [U,V]     = ginput(1);
   if U < xmin | U > xmax, break, end
   if V < ymin | V > ymax, break, end
   DIST      = KNOTEN(:,2:3);
   DIST(:,1) = DIST(:,1) - U;
   DIST(:,2) = DIST(:,2) - V;
   i         = sqrt(-1);
   DIST      = abs(DIST(:,1) + i*DIST(:,2));
   AUX       = min(DIST);
   J         = find(DIST == AUX);
   KNOTEN(J,2:3) = [U V];
   X         = KNOTEN(:,2);
   Y         = KNOTEN(:,3);
   Z         = zeros(M,1);
   clf
   axis([xmin xmax ymin ymax]);
   grid on
   hold on
   trimesh(ELEMENTE,X,Y,Z);
   hold on
   plot(X,Y,'.','MarkerSize',6);
   if isempty(RANDDAT) == 0
      plot(RANDDAT(:,1),RANDDAT(:,2),'r');
      hold on
   end
   drawnow
end
