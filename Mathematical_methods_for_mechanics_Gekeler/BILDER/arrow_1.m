function arrow_1(c,d,XMIN,XMAX,YMIN,YMAX)
% zeichnet Pfeilspitze am Punkt (X,Y),
% der durch GINPUT gegeben wird
% c ist Laenge der Pfeilspitze
% d ist Breite der Pfeilspitze an der Basis
%
while (1)
[X,Y] = ginput(2);
if X < XMIN | X > XMAX, break, end
if Y < YMIN | Y > YMAX, break, end
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
end
