function [KNOTEN,KNOTEN1] = mesh15(RAND,MAX,MIN);
% Eckart Gekeler, Universitaet Stuttgart, Release 8.4.05
% Berechnet neue Knotenzeile durch Offsetting der Normalen
% Die KNOTEN in KNOTEN2 werden nicht als neuer Rand
% verwendet
KNOTEN1 = [];
% -- Neue Zeile berechnen ---------------------------------
N      = size(RAND,2);
AUX    = [RAND(:,2:N),RAND(:,1)];
TAN    = AUX - RAND;
LN     = sqrt(TAN(1,:).*TAN(1,:) + TAN(2,:).*TAN(2,:));
TAN    = [TAN(1,:)./LN; TAN(2,:)./LN];
DIST   = (MAX + MIN)/2;
NOR    = [-TAN(2,:);TAN(1,:)];
NOR    = [NOR(1,:)./LN;NOR(2,:)./LN];
AUX    = [NOR(:,2:N),NOR(:,1)];
NOR    = NOR + AUX;
LN     = sqrt(NOR(1,:).*NOR(1,:) + NOR(2,:).*NOR(2,:));
NOR    = [NOR(1,:)./LN;NOR(2,:)./LN];
NOR    = [NOR(:,N),NOR(:,1:N-1)];
KNOTEN = RAND + DIST*NOR;
% -- zu enge Knoten beseitigen -----------------------------
N      = size(KNOTEN,2);
AUX    = [KNOTEN(:,2:N),KNOTEN(:,1)];
AUX    = AUX - KNOTEN;
LN     = sqrt(AUX(1,:).*AUX(1,:) + AUX(2,:).*AUX(2,:));
L      = find(LN >= MIN);
KNOTEN = KNOTEN(:,L);
% -- ev. neue Knoten einfuegen ----------------------------
N      = size(KNOTEN,2);
if N > 0
   J = [2:N,1]; K = [1:N];
   AUX = [KNOTEN(:,2:N),KNOTEN(:,1)];
   AUX = AUX - KNOTEN;
   LN  = sqrt(AUX(1,:).*AUX(1,:) + AUX(2,:).*AUX(2,:));
   K   = find(LN >= 1.1*MAX);
   M   = length(K);
   for L = 1:M-1
      M = size(KNOTEN,2);
      if K(L) == 1
         KNOTEN = [(KNOTEN(:,M) + KNOTEN(:,1))/2,...
                  KNOTEN(:,1:M)];
         K = K+1;
      else
         KNOTEN = [KNOTEN(:,1:K(L)-1),...
                  (KNOTEN(1:2,K(L)-1) + KNOTEN(1:2,K(L)))/2,...
                  KNOTEN(:,K(L):M)];
         K = K+1;
      end
   end
   KNOTEN = KNOTEN(1:2,:);
end
% -- Schnittpunkte berechnen ------------------------------
N = size(KNOTEN,2);
if N > 0
   PUNKTE = [];
   KNOTEN = [KNOTEN,KNOTEN(:,1)];
   for I = 1:N
      for K = I+1:N
         A = KNOTEN(1:2,I); B = KNOTEN(1:2,I+1);
         C = KNOTEN(1:2,K); D = KNOTEN(1:2,K+1);
         [FLAG,X] = schnitt(A,B,C,D);
         if FLAG == 1
            PUNKTE = [PUNKTE,[I;X(1)],[K;X(2)]];
         end
      end
   end
   if size(PUNKTE,2) > 0
      [Y,K]  = sort(PUNKTE(1,:));
      PUNKTE = PUNKTE(:,K);
      % -- Schnittpunkte einfuegen ---------------------------
      KNOTEN = [KNOTEN; ones(1,size(KNOTEN,2))];
      M      = size(PUNKTE,2);
      if M > 0
         for L = 1:M
            I = PUNKTE(1,L);   X = PUNKTE(2,L);
            A = KNOTEN(1:2,I); B = KNOTEN(1:2,I+1);
            Q = A + X*(B-A);
            KNOTEN = [KNOTEN(:,1:I),[Q;0],KNOTEN(:,I+1:size(KNOTEN,2))];
            PUNKTE(1,L+1:M) = PUNKTE(1,L+1:M)+1;
         end
      end
      % -- gew. Abschnitte zwischen Schnittpunkten loeschen --
      N  = size(KNOTEN,2); I  = find(KNOTEN(3,:) == 0);
      IL = length(I);
      if IL > 0
         % -- Sonderfall, dass 1. und letzter Schnittpunkt zusammen-
         % -- fallen, korrigieren ----------------------------------
         if norm(KNOTEN(1:2,I(1)) - KNOTEN(1:2,I(IL))) < 1.0e-08
            M      = I(2);
            KNOTEN =  [KNOTEN(:,M:N),KNOTEN(:,1:M-1)];
         end
         DONE   = 0;
         while ~DONE
            I   = min(find(KNOTEN(3,:) == 0));
            if length(I) > 0
               K = I + min(find(KNOTEN(3,I+1:N) == 0));
               X1 = KNOTEN(1:2,I); X2 = KNOTEN(1:2,I+1);
               X3 = KNOTEN(1:2,K-1);
               X21 = X2(1) - X1(1); X31 = X3(1) - X1(1);
               Y21 = X2(2) - X1(2); Y31 = X3(2) - X1(2);
               DET = X21*Y31 - X31*Y21;
               if DET <= 0
                  KNOTEN(3,I) = 1; KNOTEN(3,K) = 2;
                  KNOTEN(3,I+1:K-1) = 2;
               end
            else
               DONE = 1;
            end
         end
         J      = find(KNOTEN(3,:) ~= 2);
         KNOTEN = KNOTEN(1:2,J);
      else
         KNOTEN = KNOTEN(1:2,:);
      end
   end
end
wflag = 0;
if wflag == 1
   % -- Winkel pruefen und ev. Winkelpunkt abspalten ----------
   N      = size(KNOTEN,2);
   if N > 0
      J = [N,1:N-1]; K = [1:N]; L = [2:N,1];
      AUX1   = KNOTEN(:,J) - KNOTEN(:,K);
      LL1    = AUX1(1,:).*AUX1(1,:) + AUX1(2,:).*AUX1(2,:);
      AUX2   = KNOTEN(:,K) - KNOTEN(:,L);
      LL2    = AUX2(1,:).*AUX2(1,:) + AUX2(2,:).*AUX2(2,:);
      AUX3   = KNOTEN(:,L) - KNOTEN(:,J);
      LL3    = AUX3(1,:).*AUX3(1,:) + AUX3(2,:).*AUX3(3,:);
      CS     = (LL1 + LL2 - LL3)./(2*sqrt(LL1.*LL2));
      KNOTEN = [KNOTEN;ones(1,N)];
      for I = 1:N
         if CS(I) < 0.5*sqrt(2)
            KNOTEN1 = [KNOTEN1,KNOTEN(1:2,I)];
            KNOTEN(3,I) = 0;
         end
         if CS(I) < 0.5
            if I == 1
               KNOTEN(3,N) = 0;
               KNOTEN(1:3,2) = [(KNOTEN(1:2,2) + KNOTEN(1:2,N))/2; 1];
            end
            if I > 1 & I < N
               KNOTEN(3,I+1) = 0;
               KNOTEN(1:3,I-1) = [(KNOTEN(1:2,I-1) + KNOTEN(1:2,I+1))/2; 1];
            end
            if I == N
               KNOTEN(3,N-1) = 0;
               KNOTEN(1:3,1) = [(KNOTEN(1:2,1) + KNOTEN(1:2,N-1))/2; 1];
            end
         end
      end
      J = find(KNOTEN(3,:) ~= 0);
      KNOTEN = KNOTEN(1:2,J);
   end
end
% -- zu enge Knoten beseitigen -----------------------------
flag = 1;
if flag == 1
   N      = size(KNOTEN,2);
   if N > 0
      AUX    = [KNOTEN(:,2:N),KNOTEN(:,1)];
      AUX    = AUX - KNOTEN;
      LN     = sqrt(AUX(1,:).*AUX(1,:) + AUX(2,:).*AUX(2,:));
      L      = find(LN >= 0.7*MIN);
      KNOTEN = KNOTEN(:,L);
   end
end
