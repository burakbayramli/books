function arrow_2(c,d,X,Y)
% zeichnet Pfeilspitze am Punkt (X,Y),
% c ist Laenge der Pfeilspitze
% d ist Breite der Pfeilspitze an der Basis
%
%X = [X(1),X(2)], Y = [Y(1),Y(2)] muss eingegeben werden
VEC1 = [X(2) - X(1),Y(2) - Y(1)];
L    = norm(VEC1);
VEC1 = VEC1/L;
VEC2 = [-VEC1(2),VEC1(1)];
SPITZE = [X(2),Y(2)];
ENDE1 = SPITZE - c*VEC1 + d*VEC2/2;
ENDE2 = SPITZE - c*VEC1 - d*VEC2/2;
Z = [SPITZE; ENDE1;ENDE2;SPITZE];
fill(Z(:,1),Z(:,2),'k');
hold on
