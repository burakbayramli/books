function [rhsHx, rhsHy, rhsEz] = MaxwellHNonConRHS2D(Hx,Hy,Ez, neighbors)

% function [rhsHx, rhsHy, rhsEz] = MaxwellHNonConRHS2D(Hx,Hy,Ez, neighbors)
% Purpose  : Evaluate RHS flux in 2D Maxwell TM form 

Globals2D;

% 1.0 Only apply PEC boundary conditions at wall boundaries 
savemapB = mapB; savevmapB = vmapB; mapB = mapW;  vmapB = vmapW;

% 1.1 Evaluate right hand side 
[rhsHx, rhsHy, rhsEz] = MaxwellRHS2D(Hx, Hy, Ez);

% 1.2 Restore original boundary node lists
mapB = savemapB; vmapB = savevmapB;

% 2.0 Correct lifted fluxes at each non-conforming face fragment
Nnoncon = length(neighbors);
for n=1:Nnoncon
  neigh = neighbors{n};

  % 2.1 Extract information about this non-conforming face fragment
  k1 = neigh.elmtM; gVM = neigh.gVM;
  k2 = neigh.elmtP; gVP = neigh.gVP;
  lnx = neigh.nx;   lny = neigh.ny;        
  
  % 2.2 Compute difference of traces at Gauss nodes on face fragment
  ldHx = gVM*Hx(:,k1) - gVP*Hx(:,k2);
  ldHy = gVM*Hy(:,k1) - gVP*Hy(:,k2);
  ldEz = gVM*Ez(:,k1) - gVP*Ez(:,k2);
  
  % 2.3 Compute flux terms at Gauss nodes on face fragment
  lndotdH =  lnx.*ldHx+lny.*ldHy;
  fluxHx =  lny.*ldEz + lndotdH.*lnx-ldHx;
  fluxHy = -lnx.*ldEz + lndotdH.*lny-ldHy;
  fluxEz = -lnx.*ldHy + lny.*ldHx   -ldEz;
  
  % 2.4 Lift fluxes for non-conforming face fragments and update residuals
  lift = neigh.lift;
  rhsHx(:,k1) = rhsHx(:,k1) + lift*fluxHx/2;
  rhsHy(:,k1) = rhsHy(:,k1) + lift*fluxHy/2;
  rhsEz(:,k1) = rhsEz(:,k1) + lift*fluxEz/2;
end  
return;
