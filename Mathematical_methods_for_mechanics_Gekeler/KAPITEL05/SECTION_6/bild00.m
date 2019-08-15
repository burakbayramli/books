% BILD00, Eigenfunktion
clf
load daten UU MU0
n = 24;
U1 = UU(:,1);
%U1 = - U1;
Z = reshape(U1,n,n);
% -- Nullrandbedingungen hinzufuegen
Z = [zeros(n,1), Z, zeros(n,1)];
Z = [zeros(1,n+2);Z;zeros(1,n+2)];
clf
X     = linspace(0,1,n+2);
Y     = linspace(0,1,n+2);
[U,V] = meshgrid(X,Y);
W     = griddata(X,Y,Z,U,V,'cubic');
% -- MESH ---------------------------
mesh(U,V,W)
view(3)
