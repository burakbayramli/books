function [WBA,WBA1,MZ,MW] = wbound(p,e,t,RDW,W,Z,SEGNR,H)
% computation of boundary conditions for W

% -- Make normals --------------------- 
ecode  = 0; E = [];
for I = 1:length(SEGNR)
   J = find(e(5,:) == SEGNR(I)); E = [E,e(1:2,J)];
end
N = size(E,2);
DIFFX  = p(1,E(2,:))-p(1,E(1,:)); DIFFY = p(2,E(2,:))-p(2,E(1,:));
TAN    = [DIFFX; DIFFY]; LANG  = sqrt(DIFFX.*DIFFX + DIFFY.*DIFFY);
COSX   = DIFFX./LANG; SINX = DIFFY./LANG;
AUX  = [-SINX; COSX]; % NORMALEN
% -- Summe zweier benachbarter Normalen ---
AUX    = AUX + [AUX(:,2:N),AUX(:,1)];
LN     = sqrt(AUX(1,:).*AUX(1,:) + AUX(2,:).*AUX(2,:));
AUX    = [AUX(1,:)./LN; AUX(2,:)./LN];
AUX    = [AUX(:,N), AUX(:,1:N-1)]; % Verschieben um Eins
NORMALEN   = [E(1,:);AUX];
% -- End points of normals ------------
NORMALEN = H*NORMALEN;
XB = []; YB = [];
for I = 1:size(NORMALEN,2)
   XA = p(1,E(1,I)); YA = p(2,E(1,I));
   XB = [XB, XA + NORMALEN(2,I)]; YB = [YB, YA + NORMALEN(3,I)];
end
% -- Interpolation of Z and W ---------
X = p(1,:); Y = p(2,:); 
MEANZ = griddata(X,Y,Z',XB,YB);
MEANW = griddata(X,Y,W',XB,YB,'cubic');
% -- WBOUND -----------------
J = find(RDW(2,:) == 2);
if ~isempty(J)
   AUX   = 3*(Z(RDW(1,J))' - MEANZ(J) - RDW(3,J)*H)/H^2 - MEANW(J)/2;
   WBA1  = [RDW(1,J); AUX];
end
% -- Dirichlet boundary for W  -------------------
K     = find(RDW(2,:) == 1);
if ~isempty(K)
   WBA2  = [RDW(1,K);RDW(3,K)];
   WBA   = [WBA1,WBA2];
end
MZ = MEANZ(J); MW = MEANW(J);
