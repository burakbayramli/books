function Hrefine2D(refineflag)

% function Hrefine2D(refineflag)
% purpose:  apply non-conforming refinement to elements labelled in refineflag

Globals2D;

% 1.1 Count vertices
Nv    = length(VX(:));

% 1.2 Find and count elements to be refined
ref = sort(find(refineflag));
Nrefine = length(ref);

% 1.3 Extract vertex numbers of elements to refine
v1 = EToV(ref, 1); v2 = EToV(ref, 2); v3 = EToV(ref, 3); 

% 1.4 Uniquely number all face centers
v4 = max( 1 + Nfaces*(0:K-1)', EToF(:,1) + Nfaces*(EToE(:,1)-1) );
v5 = max( 2 + Nfaces*(0:K-1)', EToF(:,2) + Nfaces*(EToE(:,2)-1) );
v6 = max( 3 + Nfaces*(0:K-1)', EToF(:,3) + Nfaces*(EToE(:,3)-1) );

% 2.0 Extract face center vertices for elements to refine 
v4 = v4(ref);      v5 = v5(ref);      v6 = v6(ref);

% 2.1 Renumber face centers contiguously from Nv+1
ids = unique([v4;v5;v6]);
newids(ids) = (1:length(ids))';
v4 = Nv+newids(v4)'; v5 = Nv+newids(v5)'; v6 = Nv+newids(v6)';

% 2.2 Replace original triangle with triangle connecting edge centers
EToV(ref,:) = [v4,v5,v6];

% 3.0 Add extra triangles to EToV
EToV(K+1:K+3*Nrefine,1) = [v1;v2;v3]; % first  vertices of new elements
EToV(K+1:K+3*Nrefine,2) = [v4;v5;v6]; % second vertices of new elements
EToV(K+1:K+3*Nrefine,3) = [v6;v4;v5]; % third  vertices of new elements

% 3.1 Create boundary condition type for refined elements
bcsave = BCType(ref,:);
BCType(ref, :) = 0; % now internal faces

BCType(K+1:K+Nrefine, 1) = bcsave(:, 1);
BCType(K+1:K+Nrefine, 3) = bcsave(:, 3);

BCType(K+Nrefine+1:K+2*Nrefine, 1) = bcsave(:, 2);
BCType(K+Nrefine+1:K+2*Nrefine, 3) = bcsave(:, 1);

BCType(K+2*Nrefine+1:K+3*Nrefine, 1) = bcsave(:, 3);
BCType(K+2*Nrefine+1:K+3*Nrefine, 3) = bcsave(:, 2);

% 3.2 Find vertex locations of elements to be refined
x1 = VX(v1);  x2 = VX(v2);  x3 = VX(v3);    
y1 = VY(v1);  y2 = VY(v2);  y3 = VY(v3);    

% 3.3 Add coordinates for refined edge centers
VX(v4) = 0.5*(x1+x2); VX(v5) = 0.5*(x2+x3); VX(v6) = 0.5*(x3+x1); 
VY(v4) = 0.5*(y1+y2); VY(v5) = 0.5*(y2+y3); VY(v6) = 0.5*(y3+y1); 

% 3.4 Increase element count
K = K+3*Nrefine;
return;


