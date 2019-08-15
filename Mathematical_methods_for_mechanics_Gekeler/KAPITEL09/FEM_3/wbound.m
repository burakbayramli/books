function WBA = wbound(p,e,E,t,RDW,W,Z,NACHBAR)
% computation of boundary conditions for W

WBA = []; WBA1 = [];
AUX = zeros(5,size(e,2)); AUX(:,E(1,:)) = NACHBAR;
J = find(RDW(2,:) == 2);
if ~isempty(J)
   NACHBARWB = AUX(:,RDW(1,J));
   AUX = zeros(3,size(e,2)); AUX(:,RDW(1,:)) = RDW;
   RDWB = AUX(:,RDW(1,J));
   % -- computation of Wbound -----------------------
   ZR1   = Z(NACHBARWB(2,:)); ZR2 = Z(NACHBARWB(3,:));
   MEANZ = (1 - NACHBARWB(5,:)).*ZR1' + NACHBARWB(5,:).*ZR2';
   WR1   = W(NACHBARWB(2,:)); WR2 = W(NACHBARWB(3,:));
   MEANW = (1 - NACHBARWB(5,:)).*WR1' + NACHBARWB(5,:).*WR2';
   MEANL = NACHBARWB(4,:);
   AUX   = 3*(Z(RDW(1,J))' - MEANZ - RDW(3,J).*MEANL)./(MEANL.*MEANL) - MEANW/2;
   WBA  = [RDW(1,J); AUX];
end
% -- Dirichlet boundary for W  -------------------
K     = find(RDW(2,:) == 1);
if ~isempty(K)
   WBA2  = [RDW(1,K);RDW(3,K)];
   WBA   = [WBA,WBA2];
end
