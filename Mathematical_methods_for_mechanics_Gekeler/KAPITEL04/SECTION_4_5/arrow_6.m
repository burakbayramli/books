function arrow_6(X,Y,c,d,farbe,S,TYPE)
% zeichnet pfeil zw. Punkten (X(1),Y(1)) und (X(2),Y(2))
% c ist Laenge der Pfeilspitze
% d ist Breite der Pfeilspitze an der Basis
%
if TYPE == 1
plot(X,Y,'Color',farbe,'Linewidth',S);
end
if TYPE == 2
plot(X,Y,'k--','Linewidth',S);
end
hold on
VEC1 = [X(2) - X(1), Y(2) - Y(1)];
L    = norm(VEC1);
VEC1 = VEC1/L;
VEC2 = [-VEC1(2),VEC1(1)];
SPITZE = [X(2), Y(2)];
ENDE1 = SPITZE - c*VEC1 + d*VEC2/2;
ENDE2 = SPITZE - c*VEC1 - d*VEC2/2;
Z = [SPITZE; ENDE1;ENDE2;SPITZE];
fill(Z(:,1),Z(:,2),farbe);
hold on
