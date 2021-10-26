%**************************************************************************
%       Implementation of k-omega SST
%       Reference,
%       Menter, F.R., "Zonal Two equation k-omega turbulence models for 
%       aerodynamic flows", AIAA 93-2906, 1993.
%**************************************************************************
% Two equation turbulence model, which combines Wilcox k-omega model
% and k-epsilon mode through a blending funcion
%
% Conventional models without compressible modifications:
%    k-eq:  0 = Pk - (beta_star rho k om) + ddy[(mu+mut*sigma_k) dkdy]
%    om-eq: 0 = alpha rho/mut Pk - (betar rho om^2) 
%               + ddy[(mu+mut*sigma_om)domdy] + (1-BF1) CDkom
%
% Otero et.al model:
%    k-eq:  0 = Pk - (beta_star rho k om) 
%               + 1/sqrt(rho) ddy[1/sqrt(rho) (mu+mut*sigma_k) d(rho k)dy]
%    om-eq: 0 = alpha rho/mut Pk - (betar rho om^2) 
%               + ddy[1/sqrt(rho) (mu+mut*sigma_om)d(sqrt(rho) om)dy] 
%               + (1-BF1) CDkom_mod
%
% Catris, S. and Aupoix, B., "Density corrections for turbulence
%       models", Aerosp. Sci. Techn., 2000.
%    k-eq:  0 = Pk - (beta_star rho k om) 
%               + ddy[1/rho (mu+mut*sigma_k) d(rho k)dy]
%    om-eq: 0 = alpha rho/mut Pk - (betar rho om^2) 
%               + ddy[1/sqrt(rho) (mu+mut*sigma_om)d(sqrt(rho) om)dy] 
%               + (1-BF1) CDkom
%
% For simplicty, the extra density factors of the Otero et.al and Catris/Aupoix  
% models were implmeneted as extra source terms. Therefore what is solved is:
%    k-eq:  0 = Pk - (beta_star rho k om) + ddy[(mu+mut*sigma_k) dkdy]
%               + Source
%    om-eq: 0 = alpha rho/mut Pk - (betar rho om^2) 
%               + ddy[(mu+mut*sigma_om)domdy] + (1-BF1) CDkom + Source
%
% Input:
%   u           velocity
%   k           turbulent kinetic energy, from previous time step
%   om          turbulent kinetic energy dissipation rate, from previous 
%               time step
%   r           density
%   mu          molecular viscosity
%   mesh        mesh structure
%   compFlag    flag to solve the model with compressible modifications
%
% Output:
%   mut         eddy viscosity or turbulent viscosity
%   k           turbulent kinetic energy
%   om          turbulent kinetic energy dissipation rate



function [k,om,mut,CDkom,bF1,bF2] = KOmSST(u,k,om,r,mu,mesh,compFlag)

    n = size(r,1);

    % model constants
    sigma_k1  = 0.85;
    sigma_k2  = 1.0;
    sigma_om1 = 0.5;
    sigma_om2 = 0.856;
    beta_1    = 0.075;
    beta_2    = 0.0828;
    betaStar  = 0.09;
    a1        = 0.31;
    alfa_1    = beta_1/betaStar - sigma_om1*0.41^2.0/betaStar^0.5;
    alfa_2    = beta_2/betaStar - sigma_om2*0.41^2.0/betaStar^0.5;    
    
    % Relaxation factors
    underrelaxK  = 0.6;
    underrelaxOm = 0.4;
        
    % required gradients
    dkdy  = mesh.ddy*k;
    domdy = mesh.ddy*om;
    
    wallDist = min(mesh.y, 2-mesh.y);

    % VortRate = StrainRate in fully developed channel
    strMag = abs(mesh.ddy*u); 
    
    % Blending functions 
    CDkom  = 2.0*sigma_om2*r./om.*dkdy.*domdy;
    gamma1 = 500.0*mu./(r.*om.*wallDist.^2.0);
    gamma2 = 4.0*sigma_om2*r.*k./(wallDist.^2.*max(CDkom,1.0e-20));
    gamma3 = sqrt(k)./(betaStar*om.*wallDist);
    gamma  = min(max(gamma1, gamma3), gamma2);
    bF1    = tanh(gamma.^4.0); 
    gamma  = max(2.0*gamma3, gamma1);
	bF2    = tanh(gamma.^2.0); 

    % more model constants
    alfa     = alfa_1*bF1    + (1-bF1)*alfa_2;
    beta     = beta_1*bF1    + (1-bF1)*beta_2;
    sigma_k  = sigma_k1*bF1  + (1-bF1)*sigma_k2;
    sigma_om = sigma_om1*bF1 + (1-bF1)*sigma_om2;
    
    % Eddy viscosity
    zeta = min(1.0./om, a1./(strMag.*bF2));
	mut = r.*k.*zeta;
    mut = min(max(mut,0.0),100.0);

    % ---------------------------------------------------------------------
    % om-equation
    %   0 = alpha rho/mut Pk - (betar rho om^2) 
    %               + fd*ddy[mueff*d(fs*om)dy] + (1-BF1) CDkom
    
    % effective viscosity
    if compFlag >= 1 
        mueff = (mu + sigma_om.*mut)./sqrt(r);   fs = r.^0.5;
    else 
        mueff = mu + sigma_om.*mut;              fs = ones(n,1);
    end

    % diffusion matrix: mueff*d2()/dy2 + dmueff/dy d()/dy
    A =   bsxfun(@times, mueff, mesh.d2dy2) ... 
        + bsxfun(@times, mesh.ddy*mueff, mesh.ddy);
    
    % implicitly treated source term
    for i=2:n-1
        A(i,i) = A(i,i) - beta(i)*r(i)*om(i)/fs(i);
    end
    
    % Right-hand-side
    b = -alfa.*r.*strMag.^2 - (1-bF1).*CDkom;
    b = b(2:n-1);
    
    % Wall boundary conditions
    om(1) = 60.0*mu(1)/beta_1/r(1)/wallDist(2  )^2;
    om(n) = 60.0*mu(n)/beta_1/r(n)/wallDist(n-1)^2;

    % Solve
    om = solveEq(om.*fs, A, b, underrelaxOm)./fs;
    om(2:n-1) = max(om(2:n-1), 1.e-12);
    
    
    
    % ---------------------------------------------------------------------
    % k-equation
    %    0 = Pk - (beta_star rho k om) + fd*ddy[mueff*d(fs*k)dy]
    
	% effective viscosity
    switch compFlag
        case 1;    mueff = (mu + sigma_k.*mut)./sqrt(r);   
                    fs = r;   
                    fd = 1./sqrt(r);
        case 2;    mueff = (mu + sigma_k.*mut)./r;         
                    fs = r;   
                    fd = ones(n,1);
        otherwise; mueff = mu + sigma_k.*mut;       
                    fs = ones(n,1);   
                    fd = ones(n,1);
    end
       
    % diffusion matrix: mueff*d2()/dy2 + dmueff/dy d()/dy
    A =   bsxfun(@times, mueff.*fd, mesh.d2dy2) ... 
        + bsxfun(@times, (mesh.ddy*mueff).*fd, mesh.ddy);

    % implicitly treated source term
    for i=2:n-1
        A(i,i) = A(i,i) - betaStar*r(i)*om(i)/fs(i);
    end
    
    % Right-hand-side
    Pk = min(mut.*strMag.^2, 20*betaStar*k.*r.*om);
    b  = -Pk(2:n-1);
    
    % Wall boundary conditions
    k(1) = 0.0;     k(n) = 0.0;
    
    % Solve
    k = solveEq(k.*fs, A, b, underrelaxK)./fs;
    k(2:n-1) = max(k(2:n-1), 1.e-12);

end





