function YY = huelle(azimuth,elevation)
% zeichnet Kontur einer Einheitskugel
% d.h. einen Kreis unter azimuth und elevation
NN    = 100;
PHI   = (azimuth-90)*pi/180;
THETA = (elevation)*pi/180;
TT    = linspace(0,2*pi,NN);
XX    = [zeros(1,NN); cos(TT); sin(TT)];
D1    = [cos(PHI), -sin(PHI), 0; sin(PHI), cos(PHI), 0; 0, 0, 1];
D2    = [cos(THETA), 0, -sin(THETA); 0, 1, 0; sin(THETA), 0, cos(THETA)];
YY    = D2*D1*XX;
