function scheibe03
% Manuelle Konstruktion von Scheibe B aus Kreis
clf
N  = 16;      % Anzahl Intervalle
M  = [0;0];   % Mittelpunkt
R  = 5;       % Radius
FAKTOR = 1.2; % Bildgroesse
TT = pi + linspace(0,2*pi,N+1);
X  = M*ones(1,length(TT)) + R*[cos(TT);sin(TT)];
LR = [M(1)-FAKTOR*R; M(1)+FAKTOR*R];
UO = [M(2)-FAKTOR*R; M(2)+FAKTOR*R];
X  = X(:,1:size(X,2)-1);
X  = disc_aendern(X,LR,UO);
save scheibe03 M R X
