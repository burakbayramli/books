function aaglaetten
% Glaetten manuell
clear, clc, clf
%nr = 100; KK = [1,2,3,4,5,6,7];
%while ~ismember(nr,KK)
%   nr   = input(' Beispiel Nr. (1/2/3/4) ');
%end;
nr = 3;
switch nr
case 1, load daten3a X Parmeter
case 2, load daten3b X Parmeter
case 3, load daten3c X Parmeter
case 4, load daten3d X Parmeter
end
n  = Parmeter(1),
X1 = X(1:n+1); X2 = X(n+2:2*(n+1)); X3 = X(2*n+3:3*(n+1));
U  = X(3*n+4:4*(n+1));   % Kontrolle
XX = linspace(0,n,n+1);
YY = U';
KNOTEN = [XX;YY];
disp('Zum Beenden auf Rahmens tippen')
xmin = min(XX);
xmax = max(XX);
ymin = min(YY);
ymax = max(YY);
plot(KNOTEN(1,:),KNOTEN(2,:))
axis([xmin xmax ymin ymax+2]), grid on, hold on
pause
while (1)
   disp('Welchen Knoten verschieben ? ')
   [U,V]     = ginput(1);
   if U < xmin | U > xmax, break, end
   if V < ymin | V > ymax, break, end
   DIST      = KNOTEN;
   DIST(1,:) = DIST(1,:) - U;
   DIST(2,:) = DIST(2,:) - V;
   i         = sqrt(-1);
   DIST      = abs(DIST(1,:) + i*DIST(2,:));
   AUX       = min(DIST);
   J         = find(DIST == AUX);
   KNOTEN(1:2,J) = [U; V + 2*pi];
   XN         = KNOTEN(1,:);
   YN         = KNOTEN(2,:);
   clf
   axis([xmin xmax ymin ymax+2]);
   grid on
   hold on
   plot(XN,YN,'.','MarkerSize',6);
end
%grid off
X(3*n+4:4*(n+1)) = KNOTEN(2,:)';   % Kontrolle
switch nr
case 1, save daten3aa X Parmeter
case 2, save daten3bb X Parmeter
case 3, save daten3cc X Parmeter
case 4, save daten3dd X Parmeter
end

