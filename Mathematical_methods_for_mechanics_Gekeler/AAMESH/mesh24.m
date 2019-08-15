function ELEMENTE1 = mesh24(KNOTEN,ELEMENTE,U);
% Eckart Gekeler, Universitaet Stuttgart, Release 10.4.05
% berechnet zum Knotenvektor U die Dreieckselemente,
% welche die einzelnen Knoten von U enthalten
% DELAUNY-Triangulierung notwendig
% OUTPUT:
%      ELEMENTE1: ELEMENTE, die Punkte von U
%      enthalten

TRI = [];
TRI1    = ELEMENTE(1:3,:);
for I = 1:size(U,1)
   X   = KNOTEN(1,:);
   Y   = KNOTEN(2,:);
   TRI = [TRI; tsearch(X,Y,TRI1,U(1,:),U(2,:))];
end
ELEMENTE1 = ELEMENTE(TRI,:);
