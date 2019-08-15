function tangents = lanscape(p,e,segnr)
% tangents: tangents of points
% of boundary specified by segnr

X = p(1,:); Y = p(2,:);
RDAUX = [];
for I = 1:length(segnr)
   J = find(e(5,:) == segnr(I)); LJ = length(J);
   RDAUX = [RDAUX,e(1,J)];
end
RDAUX = [RDAUX,e(2,J(LJ))]; RDL = length(RDAUX);
tangents = zeros(2,RDL);
for K = 1:RDL
   KK1 = K-1; KK2 = K+1;
   if K == 1,  KK1 = K;   KK2 = K+1; end
   if K == RDL, KK1 = K-1; KK2 = K  ; end
   J1 = RDAUX(KK1); J2 = RDAUX(KK2);
   DX = X(J2)-X(J1); DY=Y(J2)-Y(J1);
   THE = atan2(DY,DX);
   tangents(:,K) = [cos(THE); sin(THE)];
end
tangents = [RDAUX;tangents];
