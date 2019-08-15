function [TANGENTS,NORMALS,OFFSET,RDEL,RDKN,E] = mesh44(p,e,t,RD);

% for boundary with segnrn in RD:
% TANGENTEN : Tangents in boundary points
% NORMALEN  : Normals in boundary points with direction into interior
% OFFSET    : Offset points in triangulation
% RDEL      : Boundary elements
% RDKN      : points in RDEL

% -- Order boundary without inner segments ---------------
E = [];
if ~isempty(RD)
   for I = 1:length(RD)
      J  = find(e(5,:) == RD(I)); EE = e(:,J);
      if any(EE(3,:)) ~= 0
         [U,K] = sort(EE(3,:)); EE = EE(:,K);
      end
      E = [E,EE];
   end
end
% -- Tangents and normals -----------------------------
N = size(E,2); TANGENTS = []; NORMALS = [];
DIFFX    = p(1,E(2,:))-p(1,E(1,:)); DIFFY = p(2,E(2,:))-p(2,E(1,:));
TAN      = [DIFFX; DIFFY];
LAENG    = sqrt(DIFFX.*DIFFX + DIFFY.*DIFFY);
COSX     = DIFFX./LAENG; SINX = DIFFY./LAENG;
NORM1    = [-SINX; COSX];
NOR      = NORM1 + [NORM1(:,2:N),NORM1(:,1)];
LN       = sqrt(NOR(1,:).*NOR(1,:) + NOR(2,:).*NOR(2,:));
NOR      = [NOR(1,:)./LN; NOR(2,:)./LN];
NOR      = [E(1,:);NOR];
NOR      = [NOR(:,N), NOR(:,1:N-1)];
NORMALS  = NOR;
TANGENTS = [E(1,:); NORMALS(3,:); - NORMALS(2,:)];

MAXG = 0; M = size(t,2); KK = zeros(15,size(E,2));
for I = 1:size(E,2)
   G = [];
   for J = 1:M
      if any(t(1:3,J) == E(1,I)), G = [G;J]; end
   end
   KK(:,I) = [G; zeros(15-length(G),1)];
   if length(G) > MAXG, MAXG = length(G); end
end
KK = KK(1:MAXG,:); RDEL = KK(:); RDEL = sort(RDEL);
for I = 1:length(RDEL)-1
       if RDEL(I) == RDEL(I+1); RDEL(I+1) = 0; end
end
J = find(RDEL ~= 0); RDEL = RDEL(J);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55
RDKN = zeros(3*MAXG,size(E,2)); MAXRL = 0;
for I = 1:size(E,2)
    RL = [];
    for K = 1:MAXG
        if KK(K,I) ~= 0, RL = [RL;t(1:3,KK(K,I))]; end
    end
    RL = sort(RL);
    for I1 = 1:length(RL)-1
       if RL(I1) == RL(I1+1); RL(I1+1) = 0; end
    end
    J = find(RL ~= 0); RL = RL(J);
    for I2 = 1:length(RL)
       for J = 1:size(E,2)
          if any(E(1,J) == RL(I2)) | any(E(2,J) == RL(I2))
             RL(I2) = 0;
          end
       end
    end
    J = find(RL ~= 0); RL = RL(J);
    if length(RL) > MAXRL, MAXRL = length(RL); end
    RDKN(:,I) = [RL; zeros(3*MAXG - length(RL),1)];
end
RDKN = RDKN(1:MAXRL,:);
% -- Randknoten muessen geordnet werden -----
for I = 1:size(E,2)
   J = find(RDKN(:,I) ~= 0);
   AUX = RDKN(J,I);
   PDIFF = p(:,AUX) - p(:,E(1,I))*ones(1,length(AUX));
   [PHI,RR] = cart2pol(PDIFF(1,:),PDIFF(2,:));
   [PSI,K] = sort(PHI);
   AUX = AUX(K);
   RDKN(J,I) = AUX;
   if max(PHI) > pi/2 & min(PHI) < - pi/2
      RDKN(J,I) = flipud(AUX);
   end
   RDKN(:,I) = flipud(RDKN(:,I));
end
RDKNAUX = RDKN(:); J = find(RDKNAUX ~= 0);
RDKNAUX = RDKNAUX(J); OFFSET = [];
for I = 2:length(RDKNAUX)
   for K = 1:I-1
      if RDKNAUX(I) == RDKNAUX(K), RDKNAUX(I) = 0; end
   end
end
J = find(RDKNAUX ~= 0); OFFSET = RDKNAUX(J)';

