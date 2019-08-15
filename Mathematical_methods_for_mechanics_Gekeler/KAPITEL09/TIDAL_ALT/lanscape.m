function [SCLAND,SCISLAND] = lanscape(p,e,RDLAND,RDISLAND)
% SCLAND: Sinus and cosinus of boundary of land
% SCISLAND: Sinus and cosinus of boundary of island
X = p(1,:); Y = p(2,:);
RDAUX = [];
for I = 1:length(RDLAND)
   J = find(e(5,:) == RDLAND(I)); LJ = length(J);
   RDAUX = [RDAUX,e(1,J)];
end
RDAUX = [RDAUX,e(2,J(LJ))]; RDL = length(RDAUX);
SCLAND = zeros(2,RDL);
for K = 1:RDL
   KK1 = K-1; KK2 = K+1;
   if K == 1,  KK1 = K;   KK2 = K+1; end
   if K == RDL, KK1 = K-1; KK2 = K  ; end
   J1 = RDAUX(KK1); J2 = RDAUX(KK2);
   DX = X(J2)-X(J1); DY=Y(J2)-Y(J1);
   THE = atan2(DY,DX);
   SCLAND(1,K) = cos(THE); SCLAND(2,K) = sin(THE);
end
SCLAND = [RDAUX;SCLAND];
% --------------------------------------
SCISLAND = [];
if ~isempty(RDISLAND)
   RDAUX = [];
   for I = 1:length(RDISLAND)
      J = find(e(5,:) == RDISLAND(I)); LJ = length(J);
      RDAUX = [RDAUX,e(1,J)];
   end
   RDAUX = [RDAUX,e(2,J(LJ))]; RDL = length(RDAUX);
   SCISLAND = zeros(2,RDL);
   for K = 1:RDL
      KK1 = K-1; KK2 = K+1;
      if K ==  1, KK1 = RDL; KK2 = K+1; end
      if K == RDL, KK1 = K-1; KK2 = 1  ; end
      J1 = RDAUX(KK1); J2 = RDAUX(KK2);
      DX = X(J2)-X(J1); DY = Y(J2)-Y(J1);
      THE = atan2(DY,DX);
      SCISLAND(1,K) = cos(THE); SCISLAND(2,K) = sin(THE);
   end
   SCISLAND = [RDAUX;SCISLAND];
end
