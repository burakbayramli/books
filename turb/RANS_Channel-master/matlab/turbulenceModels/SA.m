%**************************************************************************
%       Implementation of SA model (Spalart-Allmaras 1992 AIAA)
%       Reference,
%       Spalart, A. and Allmaras, S., "One equation turbulence model for 
%       aerodynamic flows", Recherche Aerospatiale-French edition, 1994.
%**************************************************************************
% The SA model consists of a transport equation for an eddy viscosity-like
% scalar derived using dimensional analysis, Galilean invariance and
% empiricism.
%
% Conventional models without compressible modifications:
%    nuSA-eq:  0 = cb1 Shat nuSA - cw1 fw (nuSA/wallDist)^2
%                  + 1/cb3 ddy[(nu+nuSA) dnuSAdy] + cb2/cb3 (dnuSAdy)^2
%
% Otero et.al model:
%    nuSA-eq:  0 = cb1 Shat nuSA - cw1 fw (nuSA/wallDist)^2
%                  + 1/cb3 1/rho ddy[rho (nu+nuSA) dnuSAdy]
%                  + 1/cb3 1/rho ddy[nuSA/2 (nu+nuSA) drhody]
%                  + cb2/cb3 1/rho (d(sqrt(rho) nuSA)dy)^2
%
% Catris, S. and Aupoix, B., "Density corrections for turbulence
%       models", Aerosp. Sci. Techn., 2000.
%    nuSA-eq:  0 = cb1 Shat nuSA - cw1 fw (nuSA/wallDist)^2
%                  + 1/cb3 1/rho ddy[rho (nu+nuSA) dnuSAdy]
%                  + 1/cb3 1/rho ddy[nuSA/2 (nuSA) drhody]
%                  + cb2/cb3 1/rho (d(sqrt(rho) nuSA)dy)^2
%
% For simplicty, the extra density factors of the Otero et.al and Catris/Aupoix
% models were implmeneted as extra source terms. Therefore what is solved is:
% Conventional model:
%    nuSA-eq:  0 = cb1 Shat nuSA - cw1 fw (nuSA/wallDist)^2
%                  + 1/cb3 ddy[(nu+nuSA) dnuSAdy] + cb2/cb3 (dnuSAdy)^2
%                  + Source
%
% Input:
%   u           velocity
%   nuSA        eddy viscosity-like scalar, from previous time step
%   r           density
%   mu          molecular viscosity
%   mesh        mesh structure
%   compFlag    flag to solve the model with compressible modifications
%
% Output:
%   mut         eddy viscosity or turbulent viscosity
%   nuSA        solved eddy viscosity-like scalar


function [nuSA,mut] = SA(u,nuSA,r,mu,mesh,compFlag)

    n = size(r,1);

    % Model constants
    cv1_3   = (7.1)^3;
    cb1     = 0.1355;
    cb2     = 0.622;
    cb3     = 2.0/3.0;
    inv_cb3 = 1.0/cb3;
    kappa_2 = (0.41)^2;
    cw1     = (cb1/kappa_2) + (1.0+cb2)/cb3;
    cw2     = 0.3;
    cw3_6   = (2.0)^6;

    % Relaxation factors
    underrelaxNUSA = 0.75;

    
    % pre-factors to implement compressiblity modifications for diffusion term
    % fs   ... prefactor to multiply scalar in the diffusion term
    % fd   ... prefactor for the diffusion term 
    % term ... prefactor for the diffusion term 
    % if conventional model is used, these factors are 1
    fs   = ones(n,1);
    fd   = ones(n,1);
    drho = zeros(n,1);
    
    % Coefficients
    strMag        = abs(mesh.ddy*u);    % VortRate = StrainRate in fully developed channel
    wallDist      = min(mesh.y, 2-mesh.y);
    inv_wallDist2 = min(1./wallDist.^2, 1.0e10);   % node 1 and N have INF

    chi           = nuSA.*r./mu;
    fv1           = (chi.^3)./((chi.^3) + cv1_3);
    fv2           = 1.0-(chi./(1.0+(chi.*fv1)));
    inv_kappa2_d2 = inv_wallDist2*(1.0/kappa_2);
    Shat          = strMag + inv_kappa2_d2.*fv2.*nuSA;
    inv_Shat      = 1.0./Shat;
    r_SA          = min(nuSA.*inv_kappa2_d2.*inv_Shat, 10.0);
    g             = r_SA + cw2*((r_SA.^6) - r_SA);
    g_6           = g.^6;
    fw_           = (((1.0 + cw3_6)./(g_6+ cw3_6)).^(1/6));
    fw            = g.* fw_;


    % Estimating the eddy viscosity
    mut        = zeros(n,1);
    mut(2:n-1) = fv1(2:n-1).* nuSA(2:n-1).*r(2:n-1);
    mut(2:n-1) = min(max(mut(2:n-1), 0.0), 100.0);

    % ---------------------------------------------------------------------
    % nuSA-equation 
    %    0=cb1*Shat*nuSA - cw1*fw*(nuSA/wallDist)^2 + drho
    %    + fd/cb3 ddy[nueff* d(nuSAdy)] + fd*cb2/cb3 [d(fs*nuSAdy)]^2 + ter

    % effective viscosity and diffusion of desity
    if compFlag >= 1    
            nueff = (mu./r + nuSA).*r;   fs = sqrt(r);   fd = 1./r;
            drdy  = mesh.ddy*r;
            
            % extra compressible term
            if      (compFlag==1) Di = nueff.*nuSA.*drdy;
            else                  Di = nuSA.*nuSA.*drdy;
            end
            drho = 0.5*inv_cb3./r.*(mesh.ddy*Di);
            
    else
            nueff = (mu./r + nuSA);    fs = ones(n,1);   fd = ones(n,1);
            drho  = zeros(n,1);
    end
    
    % diffusion matrix: mueff*d2()/dy2 + dmueff/dy d()/dy
    A =   bsxfun(@times, nueff.*fd, mesh.d2dy2) ... 
        + bsxfun(@times, (mesh.ddy*nueff).*fd, mesh.ddy);
    A = inv_cb3*A;
    for i=2:n-1
        A(i,i) = A(i,i) - cw1*fw(i).*nuSA(i).*inv_wallDist2(i);
    end

    % Right hand side
    dnudy = mesh.ddy*(fs.*nuSA);
    Pk = cb1*Shat(2:n-1).*nuSA(2:n-1);
    b  = - Pk - cb2* inv_cb3*fd(2:n-1).*(dnudy(2:n-1).^2) - drho(2:n-1);

    % Wall boundary conditions
    nuSA(1) = 0.0;     nuSA(n) = 0.0;

    % Solve
    nuSA = solveEq(nuSA,A,b,underrelaxNUSA);
    nuSA(2:n-1) = max(nuSA(2:n-1), 1.e-12);

end


