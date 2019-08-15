function KNOTEN = mesh08(p,e,N,SEGNR1,SEGNR2);
% Eckart Gekeler, Universitaet Stuttgart, Release 8.4.05
% p(1:2,:) : Randkoten, X-Komponenten, Y-Komponenten
% e(1:5,:) : Randkanten, uebliche Bedeutung der Zeilen
% SEGNR1 : Segmentnrn des Aussenrandes
% SEGNR2 : Segmentnrn des Innenerandes
% Erzeugt zusaetzliche aequidistante Knoten in einem
% Quadrat, welches das Gebiet mit Rand "e" enthaelt
% N + 1 Knoten in x- und in y-Richtung

TOL = 1.0e-08; %!!!!!!!!!!!!!!!
M  = size(p,2);
XMIN = min(p(1,:)); XMAX = max(p(1,:));
YMIN = min(p(2,:)); YMAX = max(p(2,:));
XDIFF = (XMAX - XMIN)/N; YDIFF = (YMAX - YMIN)/N;
X = [XMIN:XDIFF:XMAX]; Y = [YMIN:YDIFF:YMAX];
E = ones(1,N+1); KN1 = [];
for J = 1:N+1
   KN1 = [KN1, [X; Y(J)*E]];
end
% -- Knoten zu nahe an Randpunkten eliminieren ------------
L = size(KN1,2); AUX = zeros(1,L);
for I = 1:L
   for J = 1:M
      NORM1 = norm(KN1(1:2,I) - p(1:2,J));
      if NORM1 < TOL, AUX(I) = 1; end
   end
end
J = find(AUX == 0); KN1 = KN1(:,J);
% -- Knoten ausserhalb von Rand1 Streichen ---------
RAND1 = [];
for I = 1:length(SEGNR1)
   J     = find(e(5,:) == SEGNR1(I));
   RAND1 = [RAND1,[p(1,e(1,J));p(2,e(1,J))]];
end
RAND1 = [RAND1,RAND1(:,1)];
IM = inpolygon(KN1(1,:),KN1(2,:),RAND1(1,:),RAND1(2,:));
K  = find(IM == 1); KN1 = KN1(:,K);
% -- Knoten innerhalb von  Rand2  Streichen ------
if ~isempty(SEGNR2)
   RAND2 = [];
   for I = 1:length(SEGNR2)
      J     = find(e(5,:) == SEGNR2(I));
      RAND2 = [RAND2,[p(1,e(1,J));p(2,e(1,J))]];
   end
   RAND2 = [RAND2,RAND2(:,1)];
   IN = inpolygon(KN1(1,:),KN1(2,:),RAND2(1,:),RAND2(2,:));
   K  = find(IN == 0); KN1 = KN1(:,K);
end
KNOTEN = [p, KN1];
