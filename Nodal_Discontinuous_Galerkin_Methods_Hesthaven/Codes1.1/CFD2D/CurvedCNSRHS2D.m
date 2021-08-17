function [rhsQ] = CurvedCNSRHS2D(Q, mu, time, SolutionBC, fluxtype)

% function [rhsQ] = CurvedCNSRHS2D(Q, mu, time, SolutionBC, fluxtype)
% Purpose: evaluate right hand side residual of the 
%          compressible Navier-Stokes equations
Globals2D;

% Gas constant
gamma = 1.4; 

% ---------------------------------------------------------------------------

% Extract fields from three dimensional array
rho = Q(:,:,1); rhou = Q(:,:,2); rhov = Q(:,:,3); Ener = Q(:,:,4);

% Interpolate fields to volume cubature nodes & Gauss quadrature surface nodes
crho  = cub.V*rho;  grho  = gauss.interp*rho;
crhou = cub.V*rhou; grhou = gauss.interp*rhou;
crhov = cub.V*rhov; grhov = gauss.interp*rhov;
cEner = cub.V*Ener; gEner = gauss.interp*Ener;

% ---------------------------------------------------------------------------

% Compute primitive fields at Gauss quadrature surface nodes
gu = grhou./grho; gv = grhov./grho; gPr = (gamma-1)*(gEner-0.5*grho.*(gu.^2+gv.^2));  

% gmapB = []; brho = []; bu = []; bv = []; bPr = []; brhou = []; brhov = [];
if(~isempty(SolutionBC))
  % create boundary condition variables
  [brho,brhou,brhov,bEner] = feval(SolutionBC, grho, grhou, grhov, gEner, time);
  gmapB = gauss.mapB; 

  % compute primitive variables of boundary data
  bu = gu; bv = gv; bPr = gPr;
  bu(gmapB) = brhou(gmapB)./brho(gmapB);
  bv(gmapB) = brhov(gmapB)./brho(gmapB);
  bPr(gmapB) = (gamma-1)*(bEner(gmapB)-0.5*brho(gmapB).*(bu(gmapB).^2+bv(gmapB).^2));
end

% ---------------------------------------------------------------------------

% Compute gradients of the conserved variables
[drhodx,   drhody] = CurvedDGGrad2D(crho,  grho,  gmapB, brho);
[drhoudx, drhoudy] = CurvedDGGrad2D(crhou, grhou, gmapB, brhou);
[drhovdx, drhovdy] = CurvedDGGrad2D(crhov, grhov, gmapB, brhov);

% ---------------------------------------------------------------------------

% Compute primitive fields at cubature nodes & Gauss quadrature surface nodes
cu  = crhou./crho; cv  = crhov./crho; cPr = (gamma-1)*(cEner - 0.5*crho.*(cu.^2+cv.^2));

% Interpolate derivatives of conserved variables to cubature nodes
cdrhodx  = cub.V*drhodx;  cdrhody  = cub.V*drhody; 
cdrhoudx = cub.V*drhoudx; cdrhoudy = cub.V*drhoudy; 
cdrhovdx = cub.V*drhovdx; cdrhovdy = cub.V*drhovdy; 

% Use product-rule to evaluate gradients of velocity components at cubature nodes
cdudx = (cdrhoudx  - cdrhodx.*cu)./crho;
cdudy = (cdrhoudy  - cdrhody.*cu)./crho;
cdvdx = (cdrhovdx  - cdrhodx.*cv)./crho;
cdvdy = (cdrhovdy  - cdrhody.*cv)./crho;

% Compute viscous stress tensor at cubature nodes
ct11 = mu.*(2*cdudx - (2/3)*(cdudx + cdvdy)); 
ct12 = mu.*(cdudy + cdvdx);
ct22 = mu.*(2*cdvdy - (2/3)*(cdudx + cdvdy)); 
ct31 = cu.*ct11 + cv.*ct12; 
ct32 = cu.*ct12 + cv.*ct22; 

% ---------------------------------------------------------------------------

% Interpolate derivatives of conserved variables to Gauss nodes
gdrhodx  = gauss.interp*drhodx;  gdrhody  = gauss.interp*drhody; 
gdrhoudx = gauss.interp*drhoudx; gdrhoudy = gauss.interp*drhoudy; 
gdrhovdx = gauss.interp*drhovdx; gdrhovdy = gauss.interp*drhovdy; 

% Use product-rule to evaluate gradients of velocity components at Gauss nodes
gdudx = (gdrhoudx  - gdrhodx.*gu)./grho;
gdudy = (gdrhoudy  - gdrhody.*gu)./grho;
gdvdx = (gdrhovdx  - gdrhodx.*gv)./grho;
gdvdy = (gdrhovdy  - gdrhody.*gv)./grho;

% Compute viscous stress tensor at Gauss nodes
gt11 = mu.*(2*gdudx - (2/3)*(gdudx + gdvdy)); 
gt12 = mu.*(gdudy + gdvdx);
gt22 = mu.*(2*gdvdy - (2/3)*(gdudx + gdvdy)); 
gt31 = gu.*gt11 + gv.*gt12; 
gt32 = gu.*gt12 + gv.*gt22; 

% ---------------------------------------------------------------------------

% Local copy of normals at Gauss nodes 
gnx = gauss.nx; gny = gauss.ny;

% Add mass conservation terms together and compute divergence
cF = -crhou;  cG = -crhov;
gF = -grhou;  gG = -grhov;
bF = -brhou;  bG = -brhov;
rhsQ(:,:,1) = CurvedDGDiv2D(cF, cG, gF, gG, gmapB, gnx.*bF+gny.*bG);

% Add x-momentum conservation terms together and compute divergence
cF = -(crhou.*cu + cPr) + ct11; cG = -(crhou.*cv) + ct12;
gF = -(grhou.*gu + gPr) + gt11; gG = -(grhou.*gv) + gt12;
bF = -(brhou.*bu + bPr) + gt11; bG = -(brhou.*bv) + gt12;
rhsQ(:,:,2) = CurvedDGDiv2D(cF, cG, gF, gG, gmapB, gnx.*bF+gny.*bG);

% Add y-momentum conservation terms together and compute divergence
cF = -(crhou.*cv) + ct12; cG = -(crhov.*cv + cPr) + ct22;
gF = -(grhou.*gv) + gt12; gG = -(grhov.*gv + gPr) + gt22;
bF = -(brhou.*bv) + gt12; bG = -(brhov.*bv + bPr) + gt22;
rhsQ(:,:,3) = CurvedDGDiv2D(cF, cG, gF, gG, gmapB, gnx.*bF+gny.*bG);

% Add Energy conservation terms together and compute divergence
cF = cu.*(-cEner - cPr) + ct31; cG = cv.*(-cEner - cPr)  + ct32;
gF = gu.*(-gEner - gPr) + gt31; gG = gv.*(-gEner - gPr)  + gt32;
bF = bu.*(-bEner - bPr) + gt31; bG = bv.*(-bEner - bPr)  + gt32;
rhsQ(:,:,4) = CurvedDGDiv2D(cF, cG, gF, gG, gmapB, gnx.*bF+gny.*bG);

% ---------------------------------------------------------------------------

% Add Lax-Friedrichs jump stabilization
glambda = sqrt(gu.^2 + gv.^2) + sqrt(abs(gamma*gPr./grho));
glambda = max(glambda(gauss.mapM), glambda(gauss.mapP));
glambda = reshape(glambda, gauss.NGauss, Nfaces*K);
glambda = ones(gauss.NGauss, 1)*max(glambda, [], 1);
glambda = reshape(glambda, gauss.NGauss*Nfaces, K);

rhsQ(:,:,1) = rhsQ(:,:,1) + CurvedDGJump2D(glambda.*grho,  gmapB, glambda.*brho)/2;
rhsQ(:,:,2) = rhsQ(:,:,2) + CurvedDGJump2D(glambda.*grhou, gmapB, glambda.*brhou)/2;
rhsQ(:,:,3) = rhsQ(:,:,3) + CurvedDGJump2D(glambda.*grhov, gmapB, glambda.*brhov)/2;
rhsQ(:,:,4) = rhsQ(:,:,4) + CurvedDGJump2D(glambda.*gEner, gmapB, glambda.*bEner)/2;

% ---------------------------------------------------------------------------
return;
