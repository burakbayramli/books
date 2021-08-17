function [Cd, Cl, dP, sw, stri] = INSLiftDrag2D(Ux, Uy, PR, ra, nu, time, tstep, Nsteps)

% function [Cd, Cl, dP, sw, stri] = INSLiftDrag2D(Ux, Uy, PR, ra, nu, time, tstep, Nsteps)
% Purpose: compute coefficients of lift, drag and pressure drop at cylinder

Globals2D;

persistent fid sw1 sw2 stri1 stri2

if(tstep==1)
  fname = sprintf('liftdraghistory%d.dat', N);
  fid = fopen(fname, 'w');
  fprintf(fid, 'timeCdCldP = [...\n');

  % Sample location and weights for pressure drop
  % ( FINGERS CROSSED THAT WE ARE AT A VERTEX)
  [sw1, stri1] = Sample2D(-ra, 0);
  [sw2, stri2] = Sample2D( ra, 0);
end

dP = sw1*PR(:,stri1)-sw2*PR(:,stri2);

% compute derivatives
[dUxdx,dUxdy] = Grad2D(Ux); dUxdx = dUxdx(vmapC); dUxdy = dUxdy(vmapC);
[dUydx,dUydy] = Grad2D(Uy); dUydx = dUydx(vmapC); dUydy = dUydy(vmapC);

PR = PR(vmapC); nxC = nx(mapC); nyC = ny(mapC); sJC = sJ(mapC);

hforce = -PR.*nxC + nu*( nxC.*(2*dUxdx)     + nyC.*(dUydx + dUxdy) ); 
vforce = -PR.*nyC + nu*( nxC.*(dUydx+dUxdy) + nyC.*(2*dUydy)       );

hforce = reshape(hforce, Nfp, length(mapC)/Nfp);
vforce = reshape(vforce, Nfp, length(mapC)/Nfp);
sJC    = reshape(   sJC, Nfp, length(mapC)/Nfp);

% compute weights for integrating (1,hforce) and (1,vforce)
V1D = Vandermonde1D(N, r(Fmask(:,1))); 
MM1D = inv(V1D)'/V1D;
w = sum(MM1D, 1);

% Compute drag coefficient
Cd = sum(w*(sJC.*hforce));

% Compute lift coefficient
Cl = sum(w*(sJC.*vforce));

% Output answers
fprintf(fid, '%15.14f %15.14f %15.14f %15.14f;...\n', time, Cd/ra, Cl/ra, dP);
fprintf(2,   'time=%g Cd=%g Cl=%g dP=%g\n', time, Cd/ra, Cl/ra, dP);

if(tstep==Nsteps)
  fprintf(fid, '];\n');
  fclose(fid);
  fid = [];
end
return
