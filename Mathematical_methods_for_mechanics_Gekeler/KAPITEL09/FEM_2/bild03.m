function bild03
% Geometrie fuer Beispiel von BATOZ
clc, clear, format compact, format short
[KNOTEN,ELEMENTE,RD,Parmeter] = bsp04(8);
clf, hold on
plot(-1,-1,'w.'), hold on
plot(55,25,'w.'), hold on
axis equal tight, axis manual, grid on
X     = KNOTEN(1,:); Y = KNOTEN(2,:); Z = zeros(1,length(X));
xlin  = linspace(min(X),max(X),20);
ylin  = linspace(min(Y),max(Y),20);
[U,V] = meshgrid(xlin,ylin);
tri   = ELEMENTE(1:3,:)';
trimesh(tri,X,Y,Z,'edgecolor','k'), hold on
X = [0,55]; Y = [0,0]; c = 3;  d = 1;
arrow(X,Y,c,d,'k',2);
X = [0,0]; Y = [0,25]; c = 3;  d = 1;
arrow(X,Y,c,d,'k',2);
X = [0,30.48,30.48,0,0];
Y = [-1,-1,0,0,-1];
fill(X,Y,'y')
%EXAKT = [1,2,3,4,5,6; 0.754, 0.518, 0.307, 0.328, 0.142, 0.056];
J = [25, 23, 21, 15, 13, 11];
circle(KNOTEN(1,J(1)),KNOTEN(2,J(1)),0.4,'w')
circle(KNOTEN(1,J(2)),KNOTEN(2,J(2)),0.4,'w')
circle(KNOTEN(1,J(3)),KNOTEN(2,J(3)),0.4,'w')
circle(KNOTEN(1,J(4)),KNOTEN(2,J(4)),0.4,'w')
circle(KNOTEN(1,J(5)),KNOTEN(2,J(5)),0.4,'w')
circle(KNOTEN(1,J(6)),KNOTEN(2,J(5)),0.4,'w')
text(50,22,'1','fontsize',20','fontweight','demi')
text(34,22,'2','fontsize',20','fontweight','demi')
text(19,22,'3','fontsize',20','fontweight','demi')
text(42,11,'4','fontsize',20','fontweight','demi')
text(27,11,'5','fontsize',20','fontweight','demi')
text(12,11,'6','fontsize',20','fontweight','demi')

%get(h)
%plot(LOESUNG(SCHNITT))
