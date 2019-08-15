function normals = normals(p,e,p1,segnr)
% calculates normed normal in each boundary point  
% it is supposed that the boundary segments with indices in SEGNR
% are continually ordered and form a closed polygon :
% e(1,SEGNR(1)) = e(2,SEGNR(end)) 
% the 8-th row of e contains the indices of midpoints p1
% normals(1,:) : indices of boundary points in [p,p1]-matrix
% normals(2:3,:) : x- and y-coordinates of normals

% -- BOUNDARY CONDITIONS for U and V
J = find(e(5,:) == 1); LI = length(J);    % boundary segment 1 (below)
   E1 = e(1:2,J);                      % todo (left open, right open)
   MID1 = e(8,J);
   
J = find(e(5,:) == 2); LI = length(J);     % boundary segment 2 (right)
   E2 = e(1:2,J);                      % below closed, above closed 
   MID2 = e(8,J);
   
J = find(e(5,:) == 3); LI = length(J);     % boundary segment 3 (above)
   E3 = e(1:2,J);                      % right open, left open  
   MID3 = e(8,J);
   
J = find(e(5,:) == 4); LI = length(J);     % boundary segment 4 (left)
   E4 = e(1:2,J);                      % above closed, below closed 
   MID4 = e(8,J);
E = [E1,E2,E3,E4];
MIDPOINTS = [MID1,MID2,MID3,MID4];
MIDPOINTS = MIDPOINTS- size(p,2);

N = size(E,2);
DIFFX  = p(1,E(2,:))-p(1,E(1,:)); DIFFY = p(2,E(2,:))-p(2,E(1,:));
LANG  = sqrt(DIFFX.*DIFFX + DIFFY.*DIFFY);
COSX   = DIFFX./LANG; SINX = DIFFY./LANG;
AUX  = [-SINX; COSX]; % NORMALEN
AUX1 = AUX;             
% Summe zweier benachbarter Normalen
AUX    = AUX + [AUX(:,2:N),AUX(:,1)];
LN     = sqrt(AUX(1,:).*AUX(1,:) + AUX(2,:).*AUX(2,:));
AUX    = [AUX(1,:)./LN; AUX(2,:)./LN];
AUX    = [AUX(:,N), AUX(:,1:N-1)]; % Verschieben um Eins
%normals  = [E(1,:);AUX;MIDPOINTS-size(p,2);AUX1];

%normals  = [E(1,:),MIDPOINTS;-AUX,-AUX1]; %outward unit normals
normals  = [E(1,:),MIDPOINTS;AUX,AUX1]; %inward unit normals



