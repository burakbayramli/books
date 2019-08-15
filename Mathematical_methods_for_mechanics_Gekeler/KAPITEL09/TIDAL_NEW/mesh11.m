function [ELEMENTE,FORMED] = mesh11(p,e,segnr1,segnr2,GRAFIK)
% Eckart Gekeler, Universitaet Stuttgart, Release 8.4.05
% Triangulierung eines Gebietes mit Aussenrand
% und ev. einem Innenrand
% ohne innere Punkte (geht auch mit delauny)
% Vgl. Int.J.Numer.Meth.Eng. 37 (1994), pp. 1779-1789

%GRAFIK = 0; % GRAFIK = 1/0 :Zeichnung
M = 0;
for I = 1:length(segnr1)
   M = M + length(find(e(5,:) == segnr1(I)));
end
N = 0;
for I = 1:length(segnr2)
   N = N + length(find(e(5,:) == segnr2(I)));
end
J         = [1:M-1]';
TO_BE     = [J, J+1; M  1];
if N > 0
   J      = [M+1:M+N-1]';
   TO_BE  = [TO_BE; J, J+1; M+N, M+1];
end
BOUND     = TO_BE;
N2        = size(BOUND,1);
NEVER     = [TO_BE(:,2), TO_BE(:,1)];
FORMED    = [];
ELEMENTE  = [];
ISTEP     = 0;
while size(TO_BE,1) > 0
   ISTEP  = ISTEP + 1;
   K      = TO_BE(1,1);
   L      = TO_BE(1,2);
   A      = p(:,K);
   B      = p(:,L);
   LEFT_LIST = zeros(N,2);
   for J = 1:M+N
      C   = p(:,J);
      AB1 = B(1) - A(1);
      AB2 = B(2) - A(2);
      AC1 = C(1) - A(1);
      AC2 = C(2) - A(2);
      BC1 = C(1) - B(1);
      BC2 = C(2) - B(2);
      DET = AB1*AC2 - AC1*AB2;
      LEFT_LIST(J,2)...
          = sqrt(AC1^2 + AC2^2) + sqrt(BC1^2 + BC2^2);
      if DET > 0
         LEFT_LIST(J,1) = J;
      end
   end
   I         = find(LEFT_LIST(:,1) ~= 0);
   LEFT_LIST = LEFT_LIST(I,:);
   [Y,J]     = sort(LEFT_LIST(:,2));
   LEFT_LIST = LEFT_LIST(J,:);
   % -- Abarbeiten von LEFT_LIST ----------------------
   LLNR   = 1;
   done   = 0;
   if size(LEFT_LIST,1) == 0
      done = 2;
   end
   while ~done
      J   = LEFT_LIST(LLNR,1);
      C   = p(:,J);
      L1  = length(find(NEVER(:,1) == L & NEVER(:,2) == J));
      L2  = length(find(NEVER(:,1) == J & NEVER(:,2) == K));
      L3  = 0;
      L4  = 0;
      if size(FORMED,1) > 0
         L3 = length(find(FORMED(:,1) == L &...
                          FORMED(:,2) == J));
         L4 = length(find(FORMED(:,1) == J &...
                          FORMED(:,2) == K));
      end
      INLIST = L1 + L2 + L3 + L4;
      if INLIST == 0
         FLAG     = zeros(4,1);
         SCHNITT  = 0;
         for I = 1:N2
            I1    = BOUND(I,1);
            I2    = BOUND(I,2);
            D     = p(:,I1);
            E     = p(:,I2);
            FLAG(1) = schnitt(B,C,D,E);
            FLAG(2) = schnitt(C,A,D,E);
         end
         for I = 1:size(FORMED,1)
            I1    = FORMED(I,1);
            I2    = FORMED(I,2);
            D     = p(:,I1);
            E     = p(:,I2);
            FLAG(3) = schnitt(B,C,D,E);
            FLAG(4) = schnitt(C,A,D,E);
            if any(FLAG) == 1
               SCHNITT = 1;
            end
         end
         if SCHNITT == 0
            done = 1;
         end
      end
      if LLNR < size(LEFT_LIST,1);
         LLNR = LLNR + 1;
      else
         done = 2;
      end
   end
   % ----------------------------------------------------
   if done == 2
      M1    = size(TO_BE,1);
      TO_BE = TO_BE(2:M1,:);
   end
   % ----------------------------------------------------
   if done == 1
      ELEMENTE = [ELEMENTE, [K L J]'];
      % ------------------------------------------
      if GRAFIK == 1;
         %set(0,'DefaultLineColor','k');
         X1  = p(1,:); Y1  = p(2,:);
         plot(X1,Y1,'.','MarkerSize',12), hold on
         tri = ELEMENTE(1:3,:)';
         Z3  = zeros(length(X1),1);
         trimesh(tri,X1,Y1,Z3,'edgecolor','b');
         pause(0.5)
      end
      % UPDATEN VON FORMED -------------------------------
      FORMED = [FORMED; K, L; L, J; J, K];
      % UPDATEN VON TO_BE --------------------------------
      VERT   = [K, L, J, K];
      for K1 = 1:3
         N1    = size(TO_BE,1);
         AUX   = zeros(N1,1);
         for I = 1:N1
            if TO_BE(I,1) == VERT(K1) & TO_BE(I,2) == VERT(K1+1)
               AUX(I) = 1;
            end
         end
         P     = find(AUX == 0);
         Q     = find(AUX == 1);
         TO_BE = TO_BE(P,:);
         AUX   = zeros(N2,1);
         for I = 1:N2
            if BOUND(I,1) == VERT(K1) & BOUND(I,2) == VERT(K1+1)
               AUX(I) = 1;
            end
         end
         R     = find(AUX == 1);
         if length(Q) == 0  & length(R) == 0
            TO_BE = [TO_BE; VERT(K1+1), VERT(K1)];
         end
      end
   end
end
% Es treten keine doppelten Elemente auf!
