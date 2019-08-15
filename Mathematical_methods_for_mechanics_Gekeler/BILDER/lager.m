function lager(A,PHI,L)
k = 2;
% Zeichnet Lager fuer Staebe und Balken
X = [A - 0.5*L*[cos(PHI);sin(PHI)], A + 0.5*L*[cos(PHI);sin(PHI)]];
plot(X(1,:),X(2,:),'k','linewidth',k),hold on
H = L/2;
for I = 1:5
X1A = X(:,1) + (I-1)*(L/12)*(X(:,2) - X(:,1));
Y1A = X1A + H*[cos(PHI+pi/4);sin(PHI+pi/4)];
U = [X1A, Y1A];
plot(U(1,:),U(2,:),'k','linewidth',k), hold on
end
X1A = X(:,1) + 5*(L/12)*(X(:,2) - X(:,1));
Y1A = X1A + (H/2)*[cos(PHI+pi/4);sin(PHI+pi/4)];
U = [X1A, Y1A];
plot(U(1,:),U(2,:),'k','linewidth',k), hold on

X1A = X(:,1) + (L/6)*[0;1];
Y1A = X1A + (H/2)*[cos(PHI+pi/4);sin(PHI+pi/4)];
U = [X1A, Y1A];
plot(U(1,:),U(2,:),'k','linewidth',k), hold on
axis equal
