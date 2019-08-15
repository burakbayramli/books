function [NACHBAR,NORMALEN,ecode] = mesh43(p,e,segnr,OFFSET,LAENGE);
% Eckart Gekeler, Universitaet Stuttgart, Release 10.4.05
% Es wird vorausgesetzt, dass die in SEGNR zusammengefassten
% Randstuecke einen geschlossenen GEORDNETEN Rand bilden,
% Berechnet zu jedem Randpunkt die nach innen weisenden Normalenvektoren
% NACHBAR(1,:) : Nachbar C der Normalen
% NACHBAR(2,:) : Nachbar D der Normalen
% NACHBAR(3,:) : X1, Laenge der Normalen bis zum Schnitt
% NACHBAR(4,:) : X2, Schnitt C + X2*(D - C);

ecode  = 0; E = [];
for I = 1:length(segnr)
   J = find(e(5,:) == segnr(I)); E = [E,e(1:2,J)];
end
N = size(E,2);
DIFFX  = p(1,E(2,:))-p(1,E(1,:)); DIFFY = p(2,E(2,:))-p(2,E(1,:));
TAN    = [DIFFX; DIFFY]; LANG  = sqrt(DIFFX.*DIFFX + DIFFY.*DIFFY);
COSX   = DIFFX./LANG; SINX = DIFFY./LANG;
AUX  = [-SINX; COSX]; % NORMALEN
% Summe zweier benachbarter Normalen
AUX    = AUX + [AUX(:,2:N),AUX(:,1)];
LN     = sqrt(AUX(1,:).*AUX(1,:) + AUX(2,:).*AUX(2,:));
AUX    = [AUX(1,:)./LN; AUX(2,:)./LN];
AUX    = [AUX(:,N), AUX(:,1:N-1)]; % Verschieben um Eins
NORMALEN   = [E(1,:);AUX];
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ecode = 0; AUX = [OFFSET,OFFSET(1)];
NACHBAR = zeros(4,N);
for I = 1:N
    DATA = zeros(5,N);
    A = p(1:2,E(1,I)); B = A + LAENGE*NORMALEN(2:3,I);
    for K = 1:length(OFFSET)
       C = p(1:2,AUX(K)); D = p(1:2,AUX(K+1));
       [flag,X,ecode] = schnitt(A,B,C,D);
       if flag == 1
          DATA(:,K) = [flag;AUX(K);AUX(K+1);LAENGE*X(1);X(2)];
       end
    end
    J = find(DATA(1,:) == 1);
    if ~isempty(J)
       DATA = DATA(:,J);
       MINABST = min(DATA(4,:));
       L = min(find(DATA(4,:) == MINABST));
       NACHBAR(:,I) = DATA(2:5,L);
    else
   % intersection fails, take nearest offset point
   % for intersection point
      DIFF = p(:,OFFSET) - p(:,E(1,I))*ones(1,length(OFFSET));
      ABSTAND = sqrt(DIFF(1,:).^2 + DIFF(2,:).^2);
      MINABSTAND = min(ABSTAND);
      J = min(find(ABSTAND == MINABSTAND));
      NACHBAR(:,I) = [OFFSET(J);OFFSET(J);MINABSTAND;0.5];
      NORMAUX = p(:,OFFSET(J)) - p(:,E(1,I));
      NORMALEN(:,I) = [E(1,I);NORMAUX/norm(NORMAUX)];
   end
end
J = find(NACHBAR(1,:) == 0);
if ~isempty(J)
   ecode = 2;
else
   NACHBAR = [E(1,:);NACHBAR];
end

function [FLAG,X,ecode] = schnitt(A,B,C,D);
% GEKELER: FINITE ELEMENTE --------------------------
% untersucht, ob sich zwei Strecken schneiden, wenn ja
% dann S = A + X(1)*(B - A) = C + X(2)*(D - C);
ecode = 0; tol = 1.0E-8;
E = [B-A, C-D]; F = C-A; X = [-1;-1];
if abs(det(E)) > tol
   X = E\F;
else
   ecode = 2;
   if abs(F(1)- F(2)) < tol; ecode = 1; end
end
FLAG = 0;
if min(X) >= -100*eps & max(X) <= 1+100*eps, FLAG = 1; end



