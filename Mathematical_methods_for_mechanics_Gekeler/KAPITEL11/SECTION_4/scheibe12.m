% Scheibe 12, Konstruktion von Scheibe aus Kreis
clf
N  = 16;      % Anzahl Intervalle
M  = [-2;0];   % Mittelpunkt
R  = 1;       % Radius
FAKTOR = 1.2; % Bildgroesse
TT =  - linspace(0,2*pi,N+1); % Rechtsdrehung!!
X  = M*ones(1,length(TT)) + R*[cos(TT);sin(TT)];
LR = [M(1)-FAKTOR*R; M(1)+FAKTOR*R];
UO = [M(2)-FAKTOR*R; M(2)+FAKTOR*R];
X  = X(:,1:size(X,2)-1);
%axis([-2 2 4 8])
%axis equal, axis manual, grid on, hold on
%for I = 1:N
%    plot(X(1,I),X(2,I),'.k'), hold on
%    pause
%end
X  = disc_aendern(X,LR,UO);
save scheibe12 M R X
