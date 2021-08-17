function [dUdx, dUdy] = CurvedDGGrad2D(cU, gU, gmapD, bcU)

% function [dUdx, dUdy] = CurvedDGGrad2D(cU, gU, gmapD, bcU)
% Purpose: compute DG derivative of field given at cubature volume and Gauss surface nodes

Globals2D;

% Volume terms: dUdx and dUdy
dUdx = (cub.Dr')*(cub.W.*(cub.rx.*cU)) + (cub.Ds')*(cub.W.*(cub.sx.*cU));
dUdy = (cub.Dr')*(cub.W.*(cub.ry.*cU)) + (cub.Ds')*(cub.W.*(cub.sy.*cU));

% Surface traces at Gauss nodes
gUM = gU(gauss.mapM); gUP = gU(gauss.mapP);

% Apply boundary conditions
gUP(gmapD) = bcU(gmapD);
 
% Normal flux terms
fx = 0.5*(gauss.nx.*(gUM + gUP)); fy = 0.5*(gauss.ny.*(gUM + gUP));

% Add lifted flux terms to volume terms
dUdx = dUdx - gauss.interp'*(gauss.W.*fx);
dUdy = dUdy - gauss.interp'*(gauss.W.*fy);

% Multiply by inverse mass matrix templated for straight sided triangles
dUdx(:,straight) = V*V'*(dUdx(:,straight)./J(:,straight));
dUdy(:,straight) = V*V'*(dUdy(:,straight)./J(:,straight));

% Multiply by custom inverse mass matrix for each curvilinear triangle
Ncurved = length(curved);
for m=1:Ncurved
  k = curved(m);
  mmCHOL = cub.mmCHOL(:,:,k);
  dUdx(:,k) = mmCHOL\(mmCHOL'\dUdx(:,k)); dUdy(:,k) = mmCHOL\(mmCHOL'\dUdy(:,k));
end

% Correct sign
dUdx = -dUdx; dUdy = -dUdy;
return;
