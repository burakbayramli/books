function arrow_6(X,Y,c,d,farbe,S)
% zeichnet pfeil zw. Punkten
% mit den (X(1),X(2)) und (Y(1),Y(2))
% c ist Laenge der Pfeilspitze
% d ist Breite der Pfeilspitze an der Basis
%
plot([X(1),Y(1)],[X(2),Y(2)],'Color',farbe,'Linewidth',S);
hold on
VEC1 = [Y(1) - X(1), Y(2) - X(2)];
L    = norm(VEC1);
VEC1 = VEC1/L;
VEC2 = [-VEC1(2),VEC1(1)];
SPITZE = [Y(1), Y(2)];
ENDE1 = SPITZE - c*VEC1 + d*VEC2/2;
ENDE2 = SPITZE - c*VEC1 - d*VEC2/2;
Z = [SPITZE; ENDE1;ENDE2;SPITZE];
fill(Z(:,1),Z(:,2),farbe);
hold on
