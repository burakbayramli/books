function weisseln(p,e,t)
% GEKELER: FINITE ELEMENTE
% Flaeche ausserhalb von Rand (durch ''e'' definiert)
% wird geweisselt; DELAUNAY-Triangulierung
% Voraussetzung: EINE Dreiecksseite schneidet EINE Randstrecke
% uebrige Faelle unberuecksichtigt
% Rand muss geordnet werden, damit
% INPLOYGON richtig arbeitet
% -- Parameter -------------------------
N     = 5;  % weitere Punkte
DIST  = 0.5; % Distanz zum Rand in x-,y-Richtung
TOL   = 1.0E-6;
% -- HILFSPUNKTE --------------------------
X = p(1,:); Y = p(2,:);
XLIN  = linspace(min(X)-DIST,max(X)+DIST,N);
YLIN  = linspace(min(Y)-DIST,max(Y)+DIST,N);
KN    = [];
for I = 1:N
   KN = [KN, [XLIN; YLIN(I)*ones(1,N)]];
end
% -- RAND ORDNEN FUER INPOLYGON ------------
E     = [e(1,1);e(2,1)];
for I = 1:size(e,2)
   J  = find(e(1,:) == E(2,I));
   E  = [E, e(1:2,J)];
end
RAND  = p(:,E(1,:));
% -- NEUE KNOTEN  ---------------------------
KN    = [KN, RAND];
X     =  [KN(1,:), RAND(1,:)];
Y     =  [KN(2,:), RAND(2,:)];
% -- TRIANGULIERUNG; ORDNEN! ----------------
TRI   = delaunay(X,Y);
for I = 1:size(TRI,1);
   AUX = TRI(I,:);
   X21 = X(AUX(2)) - X(AUX(1));
   X31 = X(AUX(3)) - X(AUX(1));
   Y21 = Y(AUX(2)) - Y(AUX(1));
   Y31 = Y(AUX(3)) - Y(AUX(1));
   DET = X21*Y31 - X31*Y21;
   if DET <= 0
      AUX1 = TRI(I,1);
      TRI(I,1) = TRI(I,2);
      TRI(I,2) = AUX1;
   end
end
% -- SUCHEN UND ERSETZEN 1 -------------
M = size(TRI,1); AUX = zeros(1,M);
for I = 1:M
   AUX1 = inpolygon(X(TRI(I,:)),Y(TRI(I,:)),RAND(1,:),RAND(2,:));
   J   = ismember([0,0.5,1],AUX1);
   if sum(J) == 3, AUX(I) = 1; end
end
J = find(AUX == 1); L = length(J);
if L
   % --------------------------------------------
   for I = 2:L
      for K = 1:I-1
         if sum(ismember(TRI(J(I),:),TRI(J(K),:))) == 2
            [TRI1_N,TRI2_N] = change(TRI(J(I),:),TRI(J(K),:));
            TRI(J(I),:) = TRI1_N;
            TRI(J(K),:) = TRI2_N;
         end
      end
   end
end
% -- SUCHEN UND ERSETZEN 2 ------------------
AUX  = zeros(1,M);
for I = 1:M
   AUX1 = inpolygon(X(TRI(I,:)),Y(TRI(I,:)),RAND(1,:),RAND(2,:));
   if sum(AUX1) == 1 & ismember(0,AUX1)
      AUX(I) = 1;
   end
end
J = find(AUX == 1); L = length(J);
if L
   for I = 2:L
      for K = 1:I-1
         if sum(ismember(TRI(J(I),:),TRI(J(K),:))) == 2
            [C,D] = change(TRI(J(I),:),TRI(J(K),:));
            X21  = X(C(2)) - X(C(1));
            X31  = X(C(3)) - X(C(1));
            Y21  = Y(C(2)) - Y(C(1));
            Y31  = Y(C(3)) - Y(C(1));
            DET1 = X21*Y31 - X31*Y21;
            X21  = X(D(2)) - X(D(1));
            X31  = X(D(3)) - X(D(1));
            Y21  = Y(D(2)) - Y(D(1));
            Y31  = Y(D(3)) - Y(D(1));
            DET2 = X21*Y31 - X31*Y21;
            DONE1 = DET1 > TOL & DET2 > TOL;
            XNC = sum(X(C))/3;
            YNC = sum(Y(C))/3;
            XND = sum(X(D))/3;
            YND = sum(Y(D))/3;
            AUX3 = inpolygon(XNC,YNC,RAND(1,:),RAND(2,:));
            AUX4 = inpolygon(XND,YND,RAND(1,:),RAND(2,:));
            DONE2 = (AUX3==1&AUX4==0)|(AUX3==0&AUX4==1);
            if DONE1 & DONE2
               TRI(J(I),:) = C;
               TRI(J(K),:) = D;
            end
         end
      end
   end
end
% -- AUSSERE DREIECKE BERECHNEN ------------------
M  = size(TRI,1); ZN = zeros(2,M);
for I = 1:M
   ZN(1,I) = sum(X(TRI(I,:)))/3;
   ZN(2,I) = sum(Y(TRI(I,:)))/3;
end
AUX  = inpolygon(ZN(1,:),ZN(2,:),RAND(1,:),RAND(2,:));
J    = find(AUX == 0);
TRI1 = TRI(J,:);
% -- BILD 4 --------------------------------------
%trimesh(TRI,X,Y,'color','k'), hold on
Z = 0.1*ones(1,length(X));
trisurf(TRI1,X,Y,Z,'edgecolor','w','facecolor','w'), hold on
Z = 0.1*ones(1,size(RAND,2));
plot3(RAND(1,:),RAND(2,:),Z,'r'), hold on
%grid on
%axis equal

function [TRI1_N,TRI2_N] = change(TRI1,TRI2)
% GEKELER: FINITE ELEMENTE -----------------
% Hilfsfile fuer WEISSELN.M
TRI1_N = TRI1; TRI2_N = TRI2;
A = zeros(3,1); B = zeros(3,1);
AUX1 = [TRI1, TRI1([1:2])];
AUX2 = [TRI2, TRI2([1:2])];
A(1) = sum(ismember(AUX1(1:2),AUX2));
A(2) = sum(ismember(AUX1(2:3),AUX2));
A(3) = sum(ismember(AUX1(3:4),AUX2));
B(1) = sum(ismember(AUX2(1:2),AUX1));
B(2) = sum(ismember(AUX2(2:3),AUX1));
B(3) = sum(ismember(AUX2(3:4),AUX1));
I1   = find(A == 2);
I2   = find(B == 2);
if ~isempty(I1) & ~isempty(I2)
   TRI1_N = [AUX1(I1),AUX2(I2+2),AUX1(I1+2)];
   TRI2_N = [AUX2(I2),AUX1(I1+2),AUX2(I2+2)];
end



