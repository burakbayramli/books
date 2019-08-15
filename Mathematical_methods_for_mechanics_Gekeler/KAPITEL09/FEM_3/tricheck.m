% TRICHECK.M: Prueft Triangulierung

load daten  KNOTEN ELEMENTE RAND
load daten1 V W Z
clf
X     = KNOTEN(:,2);
Y     = KNOTEN(:,3);
Z1    = zeros(length(X),1);
hold on  % fuer flaches Bild --------
grid on
hold on
xlin  = linspace(min(X),max(X),20);
ylin  = linspace(min(Y),max(Y),20);
[U1,V1] = meshgrid(xlin,ylin);
W1     = griddata(X,Y,Z,U1,V1,'cubic');
tri   = ELEMENTE(:,1:3);
trimesh(tri,X,Y,Z1);
hold on
for I = 1:length(RAND) - 1
   A = [KNOTEN(RAND(I),2);KNOTEN(RAND(I+1),2)];
   B = [KNOTEN(RAND(I),3);KNOTEN(RAND(I+1),3)];
   plot(A,B,'r');
   hold on
end
plot(X,Y,'.','MarkerSize',6);
axis equal
