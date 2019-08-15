function DD = huelle(azimuth,elevation)
PHI = (azimuth-90)*pi/180;
THETA = (- elevation+60)*pi/180; % ganz gut
THETA = (elevation)*pi/180;
N = 64;
C = [cos(PHI(1))*cos(THETA(1));
     sin(PHI(1))*cos(THETA(1));
     sin(THETA(1))];
if C(1)~= 0
   D = [C(2); -C(1); 0];
else
   D = [   0; -C(3); C(2)];
end
D = D/sqrt(D'*D - 0.025);            % geschummelt!

BETA = 2*pi*[0,1:N]/N;
M = length(BETA);
DD = zeros(3,M);
for I = 1:M
   DD(:,I) = dmatrix(BETA(I),C)*D;
end
