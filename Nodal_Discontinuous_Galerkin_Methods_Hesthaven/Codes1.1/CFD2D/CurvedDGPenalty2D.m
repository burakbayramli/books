function [penaltyU] = CurvedDGPenalty2D(gU, gmapD, bcU)

% function [penaltyU] = CurvedDGPenalty2D(gU, gmapD, bcU)
% purpose: compute discontinuous Galerkin penalty applied
%          to a field given at cubature and Gauss nodes

Globals2D;

% surface traces at Gauss nodes
gUM = gU(gauss.mapM); gUP = gU(gauss.mapP); gUP(gmapD) = bcU(gmapD);

% compute element heights for each edge
hinv = 2*gauss.sJ./gauss.J; hinv = max(hinv(gauss.mapP), hinv(gauss.mapM));
NG = gauss.NGauss;
ids = (1:NG)';        ghinv(ids,:) = ones(NG, 1)*max(hinv(ids,:), [], 1);
ids = (NG+1:2*NG)';   ghinv(ids,:) = ones(NG, 1)*max(hinv(ids,:), [], 1);
ids = (2*NG+1:3*NG)'; ghinv(ids,:) = ones(NG, 1)*max(hinv(ids,:), [], 1);

% compute penalty term and lift to triangle interiors
fx = 0.5*( gUM - gUP).*ghinv*(N+1)^2;
penaltyU = -gauss.interp'*(gauss.W.*fx);

% multiply straight sided triangles by inverse mass matrix
penaltyU(:,straight) = V*V'*(penaltyU(:,straight)./J(:,straight));

% multiply by custom inverse mass matrix for each curvilinear triangle
Ncurved = length(curved);
for m=1:Ncurved
  k = curved(m);
  mmCHOL = cub.mmCHOL(:,:,k);
  penaltyU(:,k) = mmCHOL\(mmCHOL'\penaltyU(:,k));
end
return

