function arrow_8(c,d,X,PHI)
% zeichnet Pfeilspitze am Punkt (X(1),X(2)),
% der Basispunkt B wird um den Winkel PHI gedreht
% und hat den Abstand c
% c ist Laenge der Pfeilspitze
% d ist Breite der Pfeilspitze an der Basis
%
B(1) = X(1) + c*cos(PHI);
B(2) = X(2) + c*sin(PHI);
VEC1 = [B(1) - X(1),B(2) - X(2)];
L    = norm(VEC1);
VEC1 = VEC1/L;
VEC2 = [-VEC1(2),VEC1(1)];
SPITZE = [X(1),X(2)];
ENDE1 = SPITZE - c*VEC1 + d*VEC2/2;
ENDE2 = SPITZE - c*VEC1 - d*VEC2/2;
Z = [SPITZE; ENDE1;ENDE2;SPITZE];
fill(Z(:,1),Z(:,2),'k');
hold on
