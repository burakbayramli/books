function test13
% Rechnungen fuer Zahnrad
clc, clf, hold on
r = 1; R = 2; alfa = pi/4;
RADIUSAG = R*cos(alfa)
RADIUSBG = r*cos(alfa)
RADIUSAK = sqrt((R+r)^2 - R*(R+2*r)*cos(alfa)^2)
RADIUSBK = sqrt((R+r)^2 - r*(r+2*R)*cos(alfa)^2)
[th,r] = meshgrid((0:5:360)*pi/180,0:.05:1);
[X,Y]  = pol2cart(th,r);
surf(X,Y,zeros(size(X)))

