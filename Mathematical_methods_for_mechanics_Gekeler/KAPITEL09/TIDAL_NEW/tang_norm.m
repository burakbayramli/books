function [tangents,normals] = tang_norm(p,e,segnr)
% calculates unit tangents and unit normed normal at each boundary point  
% it is supposed that the boundary segments with indices in SEGNR
% are continually ordered and form a closed polygon :
% e(1,SEGNR(1)) = e(2,SEGNR(end)) 
% tangents(1,:) : indices of boundary points in node matrix p
% tangents(2:3,:) : x- and y-coordinates of tangents
% normals(1,:) : indices of boundary points in node matrix p
% normals(2:3,:) : x- and y-coordinates of normals

E = []; 
for I = 1:length(segnr)
   J = find(e(5,:) == segnr(I));
   E = [E,e(1:2,J)];
end
N = size(E,2);
DIFFX  = p(1,E(2,:))-p(1,E(1,:)); DIFFY = p(2,E(2,:))-p(2,E(1,:));
LANG  = sqrt(DIFFX.*DIFFX + DIFFY.*DIFFY);
COSX   = DIFFX./LANG; SINX = DIFFY./LANG;
AUX  = [-SINX; COSX]; % NORMALEN
% sum of two neighbored normals
AUX    = AUX + [AUX(:,2:N),AUX(:,1)];
LN     = sqrt(AUX(1,:).*AUX(1,:) + AUX(2,:).*AUX(2,:));
AUX    = [AUX(1,:)./LN; AUX(2,:)./LN];
AUX    = [AUX(:,N), AUX(:,1:N-1)]; % Verschieben um Eins
tangents = [E(1,:);AUX(2,:);-AUX(1,:)];
normals  = [E(1,:);-AUX]; %outward unit normals
%normals  = [E(1,:);AUX]; %inward unit normals



