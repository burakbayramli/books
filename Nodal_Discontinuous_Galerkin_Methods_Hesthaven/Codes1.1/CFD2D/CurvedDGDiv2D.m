function [divU] = CurvedDGDiv2D(cU, cV, gU, gV, gmapN, bcNdotU)

% function [divU] = CurvedDGDiv2D(cU, cV, gU, gV, gmapN, bcNdotU)
% Purpose: compute the divergence of a vectorial function given at cubature
%          and surface Gauss nodes

Globals2D;

% volume terms: U
divU = (cub.Dr')*(cub.W.*(cub.rx.*cU+cub.ry.*cV)) + ...
          (cub.Ds')*(cub.W.*(cub.sx.*cU+cub.sy.*cV));

% surface traces at Gauss nodes
gUM = gU(gauss.mapM); gUP = gU(gauss.mapP); 
gVM = gV(gauss.mapM); gVP = gV(gauss.mapP);

% normal fluxes
gFxM = gauss.nx.*gUM + gauss.ny.*gVM;
gFxP = gauss.nx.*gUP + gauss.ny.*gVP;

% Apply boundary conditions
gFxP(gmapN) = bcNdotU(gmapN);
 
% add flux terms to divergence
divU = divU - gauss.interp'*(gauss.W.*(gFxM + gFxP))/2;

% multiply straight sided triangles by inverse mass matrix
divU(:,straight) = V*V'*(divU(:,straight)./J(:,straight));

% multiply straight sided triangles by custom inverse mass matrices
Ncurved = length(curved);
for m=1:Ncurved
  k = curved(m);
  mmCHOL = cub.mmCHOL(:,:,k);
  divU(:,k) = mmCHOL\(mmCHOL'\divU(:,k));
end

% Correct sign
divU = -divU;
return
