%**************************************************************************
%
% RANS solver for fully developed turbulent channel with varying properties
% 
%       Created on: Nov 22, 2017
%          Authors: Rene Pecnik         (R.Pecnik@tudelft.nl)
%                   Gustavo J. Otero R. (G.J.OteroRodriguez@tudelft.nl)
%                   Process & Energy Department, Faculty of 3mE
%                   Delft University of Technology, the Netherlands.
%       Literature: Otero et al., 2017. Heat and fluid flow
% Last modified on: Jan 07, 2018
%               By: Rene Pecnik 
%**************************************************************************

close all; 
%clear all;


%--------------------------------------------------------------------------
%
% Include folders
%
addpath('mesh');                % functions for the mesh
addpath('turbulenceModels');    % functions of the turbulence models


%% ------------------------------------------------------------------------
%
% User defined inputs
%

% -----  choose test case   -----
% % constProperty  ... constant properties: rho = 1; mu = 1/ReT
% % constReTauStar ... constant semi-local Reynolds number, rho and mu variable
% % gasLike        ... gas-like fluid behaviour
% % liquidLike     ... liquid-like fluid behaviour
casename = 'constReTauStar';

% -----  choose turbulence model  -----
% 'Cess'... Cess, R.D., "A survery of the literature on heat transfer in 
%           turbulent tube flow", Tech. Rep. 8-0529-R24, Westinghouse, 1958.
% 'SA'  ... Spalart, A. and Allmaras, S., "One equation turbulence model for 
%           aerodynamic flows", Recherche Aerospatiale-French edition, 1994.
% 'MK'  ... Myong, H.K. and Kasagi, N., "A new approach to the improvement of
%           k-epsilon turbulence models for wall bounded shear flow", JSME 
%           Internationla Journal, 1990.
% 'SST' ... Menter, F.R., "Zonal Two equation k-omega turbulence models for 
%           aerodynamic flows", AIAA 93-2906, 1993.
% 'V2F' ... Medic, G. and Durbin, P.A., "Towards improved prediction of heat 
%           transfer on turbine blades", ASME, J. Turbomach. 2012.
% 'ABE' ... Abe, K. and Kondoh, T., "A new turbulence model for predicting 
%           fluid flow and heat transfer in separating and reattaching flows--1.
%           Flow field calculations", Int. J. Heat and Mass Transfer, 1994
% 'no'  ... without turbulence model; laminar
turbMod = 'ABE';

% -----  compressible modification  -----
% 0 ... Conventional models without compressible modifications
% 1 ... Otero et al.
% 2 ... Catris, S. and Aupoix, B., "Density corrections for turbulence
%       models", Aerosp. Sci. Techn., 2000.  
compMod = 1;

% -----  solve energy equation  ----- 
% 0 ... energy eq not solved, density and viscosity taken from DNS
% 1 ... energy eq solved
solveEnergy = 1;

% -----  channel height  -----
height = 2;

% -----  number of mesh points  -----
n = 120;

% -----  discretization  -----
% discr = 'finitediff' ... finite difference discretization; 
%                          requires additional parameters: 
%                          fact ... stretching factor for hyperbolic tan 
%                          ns   ... number of stencil points to the left 
%                                   and right of central differencing
%                                   scheme. So, ns = 1 is second order.
% discr = 'chebyshev' ...  Chebyshev discretization
%                          Note, this discretization is very unstable.
%                          Therefore it is best to first start with second
%                          order finite difference scheme and then switch
discr = 'finitediff';

% -----  streching factor and stencil for finite difference discretization
fact = 5;
ns = 1;



%% ------------------------------------------------------------------------
%
% Generate mesh
%
[MESH] = mesh(height, n, fact, ns, discr);


%% ------------------------------------------------------------------------
%
% Read DNS data
%
%    The DNS data is located in the folder DNS
%    The file contains info related to the case, for example: 
%    * Reynolds and Prandtl number, 
%    * exponents for temperature dependent functions of 
%      density,              rho = (T/Twall)^expRho
%      dyn. viscosity,       mu  = (T/Twall)^expMu
%      thermal conductivity, lam = (T/Twall)^expLam
%    * uniform volumetric heat source, VolQ
%    * and the DNS data. The columns of the data are described in
%      the header of the text file itself. 
%
filename = strcat('../DNS_data/',casename,'.txt');
ReTau   = dlmread(filename, '', [38 1 38 1]);
Pr      = dlmread(filename, '', [38 2 38 2]);
expRho  = dlmread(filename, '', [38 3 38 3]);
expMu   = dlmread(filename, '', [38 4 38 4]);
expLam  = dlmread(filename, '', [38 5 38 5]);
VolQ    = dlmread(filename, '', [38 6 38 6]);
DNSdata = dlmread(filename, '', 88, 0); 

% if energy is not solved, we need to interpolate the density, 
% dyn. viscosity and thermal conductivity from the DNS to the computational
% mesh. 
if (solveEnergy == 0)    
     y_DNS = [0;          DNSdata(:,1); 2-DNSdata(end:-1:1,1); 2];
     r_DNS = [1;          DNSdata(:,6);   DNSdata(end:-1:1,6); 1];
    mu_DNS = [1/ReTau;    DNSdata(:,7);   DNSdata(end:-1:1,7); 1/ReTau];
   lam_DNS = [1/ReTau/Pr; DNSdata(:,8);   DNSdata(end:-1:1,8); 1/ReTau/Pr];
    
     r = interp1(y_DNS,   r_DNS, MESH.y, 'pchip');
    mu = interp1(y_DNS,  mu_DNS, MESH.y, 'pchip');
   lam = interp1(y_DNS, lam_DNS, MESH.y, 'pchip');
end

y_DNS  = [0;          DNSdata(:,1); 2-DNSdata(end:-1:1,1); 2];
T_DNS  = interp1(y_DNS, [1      ;DNSdata(:,14);DNSdata(end:-1:1,14)     ;1], MESH.y, 'pchip');
u_DNS  = interp1(y_DNS, [0      ;DNSdata(:,10);DNSdata(end:-1:1,10)      ;0], MESH.y, 'pchip');
r_DNS  = interp1(y_DNS, [1      ;DNSdata(:,6) ;DNSdata(end:-1:1,6)       ;1], MESH.y, 'pchip');
mu_DNS = interp1(y_DNS, [1/ReTau;DNSdata(:,7) ;DNSdata(end:-1:1,7) ;1/ReTau], MESH.y, 'pchip');


%% ------------------------------------------------------------------------
%
%  Solve RANS 
%
% initialize vectors
u    = zeros(n,1);
T    = ones(n,1);

% turbulent scalars
k    = 0.1*ones(n,1);
e    = 0.001*ones(n,1);
v2   = 1/3*k;
om   = ones(n,1);
mut  = zeros(n,1);
nuSA = ones(n,1)/ReTau;

%--------------------------------------------------------------------------
%
%       Iterate RANS equations
%
nmax   = 100000;   tol  = 1.e-8;  % iteration limits
nResid = 50;                     % interval to print residuals

residual = 1e20; iter = 0;
while ((residual > tol) && iter<nmax)
    
    % solve temperature:  d/dy[(lam+mut/PrT)dTdy] = -VolQ/ReTau/Pr
    if (solveEnergy == 1)

        Prt = ones(n,1);    % simply set turbulent Prandtl number to 1

        % effective conductivity: lambda_laminar + mut/PrT
        lam = (T.^expLam)/(ReTau*Pr);
        lamEff = lam + (mut./Prt);

        % diffusion matrix: lamEff*d2phi/dy2 + dlamEff/dy dphi/dy
        A =   bsxfun(@times,          lamEff, MESH.d2dy2) ... 
            + bsxfun(@times, MESH.ddy*lamEff, MESH.ddy);

        % Isothermal BC
        T(1) = 1;
        T(n) = 1;

        % source term
        b = -VolQ*ones(n-2,1)/(ReTau*Pr);

        % Solve
        T = solveEq(T,A,b,0.95);
        
        % calculate density and viscosity from temperature
         r =  T.^expRho;
        mu = (T.^expMu)/ReTau;
    end
    

    % Solve turbulence model to calculate eddy viscosity 
    switch turbMod
        case 'V2F';  [k,e,v2,mut] = V2F(u,k,e,v2,r,mu,MESH,compMod);
        case 'MK';   [k,e,mut]    = MK(u,k,e,r,mu,ReTau,MESH,compMod);
        case 'ABE';   [k,e,mut]    = Abe(u,k,e,r,mu,ReTau,MESH,compMod);
        case 'SST';  [k,om,mut]   = KOmSST(u,k,om,r,mu,MESH,compMod);
        case 'SA';	 [nuSA,mut]   = SA(u,nuSA,r,mu,MESH,compMod);
        case 'Cess';  mut         = Cess(r,mu,ReTau,MESH,compMod);      
        otherwise;	  mut         = zeros(n,1);
    end



    % Solve momentum equation:  0 = d/dy[(mu+mut)dudy] - rho fx
    mueff = mu + mut;
   
    % diffusion matrix: mueff*d2phi/dy2 + dmueff/dy dphi/dy
    A =   bsxfun(@times, mueff, MESH.d2dy2) ... 
        + bsxfun(@times, MESH.ddy*mueff, MESH.ddy);
    
    % Right hand side
    b = -ones(n-2,1);
    
    % Solve
    u_old = u;
    u = solveEq(u,A,b,1);
    residual = norm(u-u_old);
    
    % Printing residuals
    if (mod(iter,nResid) == 0)
        fprintf('%d\t%12.6e\n', iter, residual);
    end

    iter = iter + 1;
end
fprintf('%d\t%12.6e\n\n', iter, residual);


%% ------------------------------------------------------------------------
%
%       plot RANS results and compare with DNS
%
% Set latex style
set(groot, 'DefaultTextInterpreter', 'latex');
set(groot, 'defaultAxesTickLabelInterpreter','latex');
set(groot, 'defaultLegendInterpreter','latex');

y=MESH.y;

%% ------------------------------------------------------------------------
% plotting the velocity profiles

% first calculate uplus (not really necessary, since utau = 1.0 already)
dudy   = MESH.ddy*u;
utau   = sqrt(mu(1)*dudy(1)/r(1));
upl    = u/utau;

ReTst  = ReTau*sqrt(r/r(1))./(mu/mu(1));      % semi-local Reynolds number
ypl    = y*ReTau;                             % yplus (based on wall units)
yst    = y.*ReTst;                           % ystar (based on semi-local scales)

% calculate van Driest velocity, uvd = int_0^upl (sqrt(r) dupl)
[uvd] = velTransVD(upl,r); 
    
% calculate semi-locally scaled velocity, ustar (see Patel et al. JFM 2017)
[ust] = velTransSLS(uvd, ReTst, MESH); 

figure(1); hold off 
h1 = semilogx(yst(1:n/2),ust(1:n/2),'r-','LineWidth', 2); hold on

% analytic results for viscous sub-layer
yp = linspace(0.1,13,100);
semilogx(yp,yp,'k-.');
    
% semi-empirical result for log-layer
yp = linspace(0.9,3,20);
semilogx(10.^yp, 1/0.41*log(10.^yp)+5.0,'k-.');


% DNS results 
h2 = semilogx(DNSdata(:,3),DNSdata(:,13),'ko','LineWidth', 1, 'MarkerSize', 8);

legend([h2,h1],{'DNS', 'Model'}, 'Location','northwest');
xlabel( '$y^\star$');
ylabel( '$u^\star$');
set(gca,'fontsize', 18)




%% ------------------------------------------------------------------------
% plotting the temperature profiles

if solveEnergy == 0; return; end  % only plot if energy has been solved for

dTdy  = MESH.ddy*T;
qw    = lam(1)*dTdy(1);
Ttau  = qw/(r(1)*1.0*utau);     % Note, Cp=1
Tplus = (T-T(1))/Ttau;

% van Driest transformation (same as velocity)
[Tvd] = velTransVD(Tplus, r);

% Extended van Driest transformation (same as velocity)
[Tst]  = velTransSLS(Tvd, ReTst, MESH);

figure(2); hold off 
h1 = semilogx(yst(1:n/2),Tst(1:n/2),'r-','LineWidth', 2); hold on

% DNS results
h2 = semilogx(DNSdata(:,3),DNSdata(:,18),'ko','LineWidth', 1, 'MarkerSize', 8);

legend([h2,h1],{'DNS', 'Model'}, 'Location','northwest');
xlabel( '$y^\star$');
ylabel( '$T^\star$');
set(gca,'fontsize', 18)



