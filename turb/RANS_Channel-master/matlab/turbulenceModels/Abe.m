%**************************************************************************
%       Implementation of k-epsilon Abe model
%       Reference,
%       Abe, K. and Kondoh, T., "A new turbulence model for 
%       predicting fluid flow and heat transfer in separating and 
%       reattaching flows--1. Flow field calculations", 
%       Int. J. Heat and Mass Transfer, 1994
%**************************************************************************
% An improved near-wall k-epsilon turbulence model that considers two 
% characteristics lenght scale for dissipation rate.
%
% Conventional models without compressible modifications:
%    k-eq:  0 = Pk - rho e + ddy[(mu+mut/sigma_k) dkdy]
%    e-eq:  0 = C_e1 f1 e/k Pk - C_e2 f2 e^2/k + ddy[(mu+mut/sigma_e)dedy] 
%
% Otero et.al model:
%    k-eq:  0 = Pk - rho e
%               + 1/sqrt(rho) ddy[1/sqrt(rho) (mu+mut/sigma_k) d(rho k)dy]
%    e-eq:  0 = C_e1 f1 e/k Pk - C_e2 f2 e^2/k 
%               + 1/rho ddy[1/sqrt(rho) (mu+mut/sigma_e) d(rho^1.5 e)dy] 
% This models uses "yplus". It must be replace by its semi-locally scaled
% counter part "ystar"
%
% Catris, S. and Aupoix, B., "Density corrections for turbulence
%       models", Aerosp. Sci. Techn., 2000.
%    k-eq:  0 = Pk - rho e 
%               + ddy[1/rho (mu+mut/sigma_k) d(rho k)dy]
%    e-eq:  0 = C_e1 f1 e/k Pk - C_e2 f2 e^2/k 
%               + 1/rho ddy[1/sqrt(rho) (mu+mut/sigma_e) d(rho^1.5 e)dy]
%
% For simplicty, the extra density factors of the Otero et.al and Catris/Aupoix  
% models were implmeneted as extra source terms. Therefore what is solved is:
%    k-eq:  0 = Pk -  rho e + ddy[(mu+mut/sigma_k) dkdy] + Source
%    e-eq:  0 = C_e1 f1 e/k Pk - C_e2 f2 e^2/k + ddy[(mu+mut/sigma_e)dedy] 
%               + Source
%
% Input:
%   u           velocity
%   k           turbulent kinetic energy, from previous time step
%   e           turbulent kinetic energy dissipation rate per unit volume,  
%               from previous time step
%   r           density
%   mu          molecular viscosity
%   ReT         friction Reynolds number ReT=utau r_wall h/ mu_wall
%   mesh        mesh structure
%   compFlag    flag to solve the model with compressible modifications
%
% Output:
%   mut         eddy viscosity or turbulent viscosity
%   k           solved turbulent kinetic energy
%   e           solved turbulent kinetic energy dissipation rate per unit
%               volume

function [k,e,mut] = Abe(u,k,e,r,mu,ReT,mesh,compFlag)

    n        = size(r,1);
    y        = mesh.y;
    wallDist = min(y, 2-y);

    % Model constants
    cmu  = 0.09; 
    sigk = 1.4; 
    sige = 1.4; 
    Ce1  = 1.45; 
    Ce2  = 1.9;
    
    % Relaxation factor
    underrelaxK  = 0.8;
    underrelaxE  = 0.8;

    % ---------------------------------------------------------------------
    % eddy viscosity
    ReTurb = r.*(k.^2)./(mu.*e);
    y_star = (wallDist.*((e.*(mu./r)).^0.25))./(mu./r);
    fmue = ((1-exp(-y_star/14)).^2).*(1+(5./(ReTurb.^0.75)).*exp(-(ReTurb./200).^2));
    fmue(1:n-1:n) = 0.0;
    feps = (1-exp(-y_star/3.1)).^2.*(1-0.3*exp(-(ReTurb./6.5).^2));
    feps(1:n-1:n) = 0.0;

    mut  = r*cmu.*fmue.*k.^2./e;
    mut(2:n-1) = min(max(mut(2:n-1),1.0e-10),100.0);
    
    % ---------------------------------------------------------------------
    % Turbulent production
    dudy = mesh.ddy*u;
    Pk   = mut.*dudy.^2;


    % ---------------------------------------------------------------------
    % e-equation
    %    0 = C_e1 f1 e/k Pk - C_e2 f2 e^2/k + fd*ddy[mueff*d(fs*e)dy] 
    
    % effective viscosity
    if compFlag >= 1 
        mueff = (mu + mut/sige)./sqrt(r);   fs = r.^1.5;   fd = 1./r;
    else 
        mueff = mu + mut/sige;           fs = ones(n,1);   fd = ones(n,1);
    end

    % diffusion matrix: mueff*d2()/dy2 + dmueff/dy d()/dy
    A =   bsxfun(@times, mueff.*fd, mesh.d2dy2) ... 
        + bsxfun(@times, (mesh.ddy*mueff).*fd, mesh.ddy);

    % Left-hand-side, implicitly treated source term
    for i=2:n-1
        A(i,i) = A(i,i) - Ce2*feps(i)*r(i)*e(i)/k(i)/fs(i);
    end

    % Right-hand-side
    b = -e(2:n-1)./k(2:n-1).*Ce1.*Pk(2:n-1);
    
    % Wall boundary conditions
    e(1) = 2*mu(1)/r(1)*sqrt(k(2  ))/wallDist(2  )^2;
    e(n) = 2*mu(n)/r(n)*sqrt(k(n-1))/wallDist(n-1)^2;

    % Solve eps equation
    e = solveEq(e.*fs, A, b, underrelaxE)./fs;
    e(2:n-1) = max(e(2:n-1), 1.e-12);
    

    % ---------------------------------------------------------------------
    % k-equation
    %    0 = Pk - rho e + fd*ddy[mueff*d(fs*k)dy]

    % effective viscosity
    switch compFlag
        case 1;    mueff = (mu + mut/sigk)./sqrt(r);   fs = r;   fd = 1./sqrt(r);
        case 2;    mueff = (mu + mut/sigk)./r;         fs = r;   fd = ones(n,1);
        otherwise; mueff = mu + mut/sigk;      fs = ones(n,1);   fd = ones(n,1);
    end

    % diffusion matrix: mueff*d2()/dy2 + dmueff/dy d()/dy
    A =   bsxfun(@times, mueff.*fd, mesh.d2dy2) ... 
        + bsxfun(@times, (mesh.ddy*mueff).*fd, mesh.ddy);
    
    % implicitly treated source term
    for i=2:n-1
        A(i,i) = A(i,i) - r(i).*e(i)./k(i)/fs(i);
    end
    
    % Right-hand-side
    b  = -Pk(2:n-1);
    
    % Solve TKE
    k = solveEq(k.*fs, A, b, underrelaxK)./fs;
    k(2:n-1) = max(k(2:n-1), 1.e-12);

end
