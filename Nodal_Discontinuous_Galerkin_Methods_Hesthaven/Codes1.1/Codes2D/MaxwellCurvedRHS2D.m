function [rhsHx, rhsHy, rhsEz] = MaxwellCurvedRHS2D(cinfo, Hx,Hy,Ez)

% function [rhsHx, rhsHy, rhsEz] = MaxwellCurvedRHS2D(cinfo, Hx,Hy,Ez)
% Purpose  : Evaluate RHS flux in 2D Maxwell TM form 

Globals2D;

[rhsHx,rhsHy,rhsEz] = MaxwellRHS2D(Hx, Hy, Ez);

% correct residuals at each curved element
Ncinfo = length(cinfo);
for n=1:Ncinfo

  % for each curved element computed L2 derivatives via cubature
  cur = cinfo(n); k1 = cur.elmt; cDx = cur.Dx; cDy = cur.Dy; 
  
  rhsHx(:,k1) = -cDy*Ez(:,k1);
  rhsHy(:,k1) =  cDx*Ez(:,k1);
  rhsEz(:,k1) =  cDx*Hy(:,k1) - cDy*Hx(:,k1);

  % for each face of each curved element use Gauss quadrature based lifts
  for f1=1:Nfaces
    k2 = EToE(k1,f1);
    gnx = cur.gnx(:,f1); gny = cur.gny(:,f1);
    gVM = cur.gVM(:,:,f1); gVP = cur.gVP(:,:,f1);
    glift = cur.glift(:,:,f1);

    % compute difference of solution traces at Gauss nodes
    gdHx = gVM*Hx(:,k1) - gVP*Hx(:,k2);
    gdHy = gVM*Hy(:,k1) - gVP*Hy(:,k2);
    gdEz = gVM*Ez(:,k1) - gVP*Ez(:,k2);

    % correct jump at Gauss nodes on domain boundary faces
    if(k1==k2)
      gdHx = 0*gdHx; gdHy = 0*gdHy; gdEz = 2*gVM*Ez(:,k1);
    end

    % perform upwinding
    gndotdH =  gnx.*gdHx+gny.*gdHy;
    fluxHx =  gny.*gdEz + gndotdH.*gnx-gdHx;
    fluxHy = -gnx.*gdEz + gndotdH.*gny-gdHy;
    fluxEz = -gnx.*gdHy + gny.*gdHx   -gdEz;

    % lift flux terms using Gauss based lift operator
    rhsHx(:,k1) = rhsHx(:,k1) + glift*fluxHx/2;
    rhsHy(:,k1) = rhsHy(:,k1) + glift*fluxHy/2;
    rhsEz(:,k1) = rhsEz(:,k1) + glift*fluxEz/2;
  end  
end
return;
