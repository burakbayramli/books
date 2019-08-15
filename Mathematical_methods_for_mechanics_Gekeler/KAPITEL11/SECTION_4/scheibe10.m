% Scheibe 10, Konstruktion von Scheibe aus Kreis
clf
N  = 16;      % Anzahl Intervalle
M  = [-6;0];   % Mittelpunkt
R  = 1;       % Radius
FAKTOR = 1.2; % Bildgroesse
TT = - linspace(0,2*pi,N+1);
X  = M*ones(1,length(TT)) + R*[cos(TT);sin(TT)];
LR = [M(1)-FAKTOR*R; M(1)+FAKTOR*R];
UO = [M(2)-FAKTOR*R; M(2)+FAKTOR*R];
X  = X(:,1:size(X,2)-1);
X  = disc_aendern(X,LR,UO);
save scheibe10 M R X
