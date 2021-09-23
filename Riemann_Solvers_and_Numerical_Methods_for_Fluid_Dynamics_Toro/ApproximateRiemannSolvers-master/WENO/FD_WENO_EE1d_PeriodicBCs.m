function res = FD_WENO_EE1d_PeriodicBCs(a,q,~,dx,fsplitMth,Recon,~)
% *************************************************************************
%
%          Vertor Flux Splitting solver for system of equations
%                   with periodic boundary conditions
%
% Coded by Manuel A. Diaz, 02.10.2012, NTU Taiwan.
% Last update on 2016.04.29, NHRI Taiwan.
% *************************************************************************

% 1. Produce flux splitting 
switch fsplitMth
    case 'LF',  [fp,fm] = LF(a,q);    % Lax-Friedrichs (LF) Flux Splitting
    case 'LLF', [fp,fm] = Rusanov(q); % Rusanov (Rus) Flux Splitting
    case 'SHLL',[fp,fm] = SHLL(q);    % Split HLL (SHLL) flux 
    otherwise, error('Splitting method not set.');
end

% 2. Produce reconstructions
switch Recon
    case 'WENO5', [hn,hp] = WENO5recon(fp,fm);
    case 'WENO7', [hn,hp] = WENO7recon(fp,fm);
    case 'Poly5', [hn,hp] = POLY5recon(fp,fm);
    case 'Poly7', [hn,hp] = POLY7recon(fp,fm);
    otherwise, error('reconstruction not available ;P');
end

% 3. Compute finite difference residual term, df/dx.
res = (hp+hn-circshift(hp+hn,[0,1]))/dx;
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Flux splitting functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Lax-Friedrichs
function [Fp,Fm] = LF(a,q)
    global gamma
    
    % primary properties
    rho=q(1,:); Fm=q(2,:)./rho; E=q(3,:)./rho; 
    p=(gamma-1)*rho.*(E-0.5*Fm.^2);
    
    % flux vector of conserved properties
    F=[rho.*Fm; rho.*Fm.^2+p; Fm.*(rho.*E+p)];
    
    % Lax-Friedrichs flux
    Fp=0.5*(F + a*q); 
    Fm=0.5*(F - a*q); 
end

% Rusanov (or local Lax-Friedrichs)
function [Fp,Fm] = Rusanov(q)
    global gamma
    
    % primary properties
    rho=q(1,:); u=q(2,:)./rho; E=q(3,:)./rho; 
    p=(gamma-1)*rho.*(E-0.5*u.^2); a=sqrt(gamma*p./rho); 
    
    % flux vector of conserved properties
    F=[rho.*u; rho.*u.^2+p; u.*(rho.*E+p)];
    
    % positive and negative fluxes
    I=ones(3,1); % I = [1;1;1;] column vector
    Fp=0.5*(F + I*a.*q); 
    Fm=0.5*(F - I*a.*q); 
end

% Splitted HLL flux form Ref.[2]:
function [Fp,Fm] = SHLL(q)
    global gamma
    
    % primary properties
    rho=q(1,:); u=q(2,:)./rho; E=q(3,:)./rho; 
    p=(gamma-1)*rho.*(E-0.5*u.^2);
    
    % flux vector of conserved properties
    F=[rho.*u; rho.*u.^2+p; u.*(rho.*E+p)];
    
    % Mach number
    a=sqrt(gamma*p./rho); M = u./a; 
    
    % Produce corrections to Mach number
    M(M> 1)= 1; 
    M(M<-1)=-1;
    M2 = M.^2;
    
    % constant column vector [1;1;1]
    I = ones(3,1);
    Fp= 0.5*((I*(M+1)).*F + I*(a.*(1-M2)).*q); 
    Fm=-0.5*((I*(M-1)).*F + I*(a.*(1-M2)).*q); 
end

%%%%%%%%%%%%%%%%%%
% Reconstructions
%%%%%%%%%%%%%%%%%%

function [hn,hp] = WENO5recon(v,u)
% *************************************************************************
% Based on:
% C.W. Shu's Lectures notes on: 'ENO and WENO schemes for Hyperbolic
% Conservation Laws' 
%
% coded by Manuel Diaz, 02.10.2012, NTU Taiwan.
% last update on 2016.04.29, NHRI Taiwan.
% *************************************************************************
%
% Domain cells (I{i}) reference:
%
%                |           |   u(i)    |           |
%                |  u(i-1)   |___________|           |
%                |___________|           |   u(i+1)  |
%                |           |           |___________|
%             ...|-----0-----|-----0-----|-----0-----|...
%                |    i-1    |     i     |    i+1    |
%                |-         +|-         +|-         +|
%              i-3/2       i-1/2       i+1/2       i+3/2
%
% ENO stencils (S{r}) reference:
%
%
%                           |___________S2__________|
%                           |                       |
%                   |___________S1__________|       |
%                   |                       |       |
%           |___________S0__________|       |       |
%         ..|---o---|---o---|---o---|---o---|---o---|...
%           | I{i-2}| I{i-1}|  I{i} | I{i+1}| I{i+2}|
%                                  -|
%                                 i+1/2
%
%
%                   |___________S0__________|
%                   |                       |
%                   |       |___________S1__________|
%                   |       |                       |
%                   |       |       |___________S2__________|
%                 ..|---o---|---o---|---o---|---o---|---o---|...
%                   | I{i-1}|  I{i} | I{i+1}| I{i+2}| I{i+3}|
%                                   |+
%                                 i+1/2
%
% WENO stencil: S{i} = [ I{i-2},...,I{i+3} ]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Careful!: by using circshift over our domain, we are implicitly creating a
% favorable code that automatically includes periodical boundary conditions.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Extrapolation $v_{i+1/2}^{-}$ == $f_{i+1/2}^{+}$
vmm = circshift(v,[0 2]);
vm  = circshift(v,[0 1]);
%v  = circshift(v,[0 0]);
vp  = circshift(v,[0 -1]);
vpp = circshift(v,[0 -2]);

% Polynomials
p0n = (2*vmm - 7*vm + 11*v)/6;
p1n = ( -vm  + 5*v  + 2*vp)/6;
p2n = (2*v   + 5*vp - vpp )/6;

% Smooth Indicators (Beta factors)
B0n = 13/12*(vmm-2*vm+v  ).^2 + 1/4*(vmm-4*vm+3*v).^2; 
B1n = 13/12*(vm -2*v +vp ).^2 + 1/4*(vm-vp).^2;
B2n = 13/12*(v  -2*vp+vpp).^2 + 1/4*(3*v-4*vp+vpp).^2;

% Constants
d0n = 1/10; d1n = 6/10; d2n = 3/10; epsilon = 1e-6;

% Alpha weights 
alpha0n = d0n./(epsilon + B0n).^2;
alpha1n = d1n./(epsilon + B1n).^2;
alpha2n = d2n./(epsilon + B2n).^2;
alphasumn = alpha0n + alpha1n + alpha2n;

% ENO stencils weigths
w0n = alpha0n./alphasumn;
w1n = alpha1n./alphasumn;
w2n = alpha2n./alphasumn;

% Numerical Flux at cell boundary, $v_{i+1/2}^{-}$;
hn = w0n.*p0n + w1n.*p1n + w2n.*p2n;

%% Extrapolation $u_{i+1/2}^{+}$ == $f_{i+1/2}^{-}$
umm = circshift(u,[0 1]);
um  = circshift(u,[0 0]);
uo  = circshift(u,[0 -1]);
up  = circshift(u,[0 -2]);
upp = circshift(u,[0 -3]);

% Polynomials
p0p = ( -umm + 5*um + 2*uo  )/6;
p1p = ( 2*um + 5*uo  - up   )/6;
p2p = (11*uo  - 7*up + 2*upp)/6;

% Smooth Indicators (Beta factors)
B0p = 13/12*(umm-2*um+uo  ).^2 + 1/4*(umm-4*um+3*uo).^2; 
B1p = 13/12*(um -2*uo +up ).^2 + 1/4*(um-up).^2;
B2p = 13/12*(uo  -2*up+upp).^2 + 1/4*(3*uo -4*up+upp).^2;

% Constants
d0p = 3/10; d1p = 6/10; d2p = 1/10; epsilon = 1e-6;

% Alpha weights 
alpha0p = d0p./(epsilon + B0p).^2;
alpha1p = d1p./(epsilon + B1p).^2;
alpha2p = d2p./(epsilon + B2p).^2;
alphasump = alpha0p + alpha1p + alpha2p;

% ENO stencils weigths
w0p = alpha0p./alphasump;
w1p = alpha1p./alphasump;
w2p = alpha2p./alphasump;

% Numerical Flux at cell boundary, $u_{i-1/2}^{+}$;
hp = w0p.*p0p + w1p.*p1p + w2p.*p2p;
end

function [hn,hp] = WENO7recon(v,u)
% *************************************************************************
% Based on:
% C.W. Shu's Lectures notes on: 'ENO and WENO schemes for Hyperbolic
% Conservation Laws' 
%
% coded by Manuel Diaz, 02.10.2012, NTU Taiwan.
% *************************************************************************
%
% Domain cells (I{i}) reference:
%
%                |           |   u(i)    |           |
%                |  u(i-1)   |___________|           |
%                |___________|           |   u(i+1)  |
%                |           |           |___________|
%             ...|-----0-----|-----0-----|-----0-----|...
%                |    i-1    |     i     |    i+1    |
%                |-         +|-         +|-         +|
%              i-3/2       i-1/2       i+1/2       i+3/2
%
% ENO stencils (S{r}) reference:
%
%                               |_______________S3______________|
%                               |                               |
%                       |______________S2_______________|       |
%                       |                               |       |
%               |______________S1_______________|       |       |
%               |                               |       |       |
%       |_______________S0______________|       |       |       |
%     ..|---o---|---o---|---o---|---o---|---o---|---o---|---o---|...
%       | I{i-3}| I{i-2}| I{i-1}|  I{i} | I{i+1}| I{i+2}| I{i+3}|
%                                      -|
%                                     i+1/2
%
%       |______________S0_______________|
%       |                               |
%       |       |______________S1_______________|
%       |       |                               |
%       |       |       |______________S2_______________|
%       |       |       |                               |
%       |       |       |       |_______________S3______________|
%     ..|---o---|---o---|---o---|---o---|---o---|---o---|---o---|...
%       | I{i-3}| I{i-2}| I{i-1}|  I{i} | I{i+1}| I{i+2}|| I{i+3}
%                               |+
%                             i-1/2
%
% WENO stencil: S{i} = [ I{i-3},...,I{i+3} ]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Careful!: by using circshift over our domain, we are implicitly creating a
% favorable code that automatically includes periodical boundary conditions.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Extrapolation $v_{i+1/2}^{-}$ == $f_{i+1/2}^{+}$
vmmm= circshift(v,[0 3]);
vmm = circshift(v,[0 2]);
vm  = circshift(v,[0 1]);
%v  = circshift(v,[0 0]);
vp  = circshift(v,[0 -1]);
vpp = circshift(v,[0 -2]);
vppp= circshift(v,[0 -3]);

% Smooth Indicators
B0n = vm.*(134241*vm-114894*v)   +vmmm.*(56694*vm-47214*vmm+6649*vmmm-22778*v)...
        +25729*v.^2  +vmm.*(-210282*vm+85641*vmm+86214*v);
B1n = v.*(41001*v-30414*vp)     +vmm.*(-19374*vm+3169*vmm+19014*v-5978*vp)...
        +6649*vp.^2   +vm.*(33441*vm-70602*v+23094*vp);
B2n = vp.*(33441*vp-19374*vpp)    +vm.*(6649*vm-30414*v+23094*vp-5978*vpp)...
        +3169*vpp.^2  +v.*(41001*v-70602*vp+19014*vpp);
B3n = vpp.*(85641*vpp-47214*vppp) +v.*(25729*v-114894*vp+86214*vpp-22778*vppp)...
        +6649*vppp.^2 +vp.*(134241*vp-210282*vpp+56694*vppp);

% Constants
g0 = 1/35; g1 = 12/35; g2 = 18/35; g3 = 4/35; epsilon = 1e-6;

% Alpha weights
alpha0n = g0./(epsilon + B0n).^2;
alpha1n = g1./(epsilon + B1n).^2;
alpha2n = g2./(epsilon + B2n).^2;
alpha3n = g3./(epsilon + B3n).^2;
alphasumn = alpha0n + alpha1n + alpha2n + alpha3n;

% Non-linear weigths
w0n = alpha0n./alphasumn;
w1n = alpha1n./alphasumn;
w2n = alpha2n./alphasumn;
w3n = alpha3n./alphasumn;

% Numerical Flux at cell boundary, $v_{i+1/2}^{-}$;
hn = w0n.*(-3*vmmm + 13*vmm - 23*vm  + 25*v  )/12 + ...
     w1n.*( 1*vmm  - 5*vm   + 13*v  +  3*vp  )/12 + ...
     w2n.*(-1*vm   + 7*v   +  7*vp  -  1*vpp )/12 + ...
     w3n.*( 3*v   + 13*vp  -  5*vpp +  1*vppp)/12;

%% Extrapolation $u_{i+1/2}^{+}$ == $f_{i+1/2}^{-}$
ummm= circshift(u,[0 2]);
umm = circshift(u,[0 1]);
um  = circshift(u,[0 0]);
uo  = circshift(u,[0 -1]);
up  = circshift(u,[0 -2]);
upp = circshift(u,[0 -3]);
uppp= circshift(u,[0 -4]);

% Smooth Indicators
B0p = um.*(134241*um-114894*uo)   +ummm.*(56694*um-47214*umm+6649*ummm-22778*uo)...
        +25729*uo.^2  +umm.*(-210282*um+85641*umm+86214*uo);
B1p = uo.*(41001*uo-30414*up)     +umm.*(-19374*um+3169*umm+19014*uo-5978*up)...
        +6649*up.^2   +um.*(33441*um-70602*uo+23094*up);
B2p = up.*(33441*up-19374*upp)    +um.*(6649*um-30414*uo+23094*up-5978*upp)...
        +3169*upp.^2  +uo.*(41001*uo-70602*up+19014*upp);
B3p = upp.*(85641*upp-47214*uppp) +uo.*(25729*uo-114894*up+86214*upp-22778*uppp)...
        +6649*uppp.^2 +up.*(134241*up-210282*upp+56694*uppp);

% Constants
g0 = 4/35; g1 = 18/35; g2 = 12/35; g3 = 1/35; epsilon = 1e-6;

% Alpha weights
alpha0p = g0./(epsilon + B0p).^2;
alpha1p = g1./(epsilon + B1p).^2;
alpha2p = g2./(epsilon + B2p).^2;
alpha3p = g3./(epsilon + B3p).^2;
alphasump = alpha0p + alpha1p + alpha2p + alpha3p;

% Non-linear weigths
w0p = alpha0p./alphasump;
w1p = alpha1p./alphasump;
w2p = alpha2p./alphasump;
w3p = alpha3p./alphasump;

% Numerical Flux at cell boundary, $u_{i+1/2}^{+}$;
hp = w0p.*( 1*ummm - 5*umm  + 13*um  +  3*uo  )/12 + ...
     w1p.*(-1*umm  + 7*um   +  7*uo  -  1*up  )/12 + ... 
     w2p.*( 3*um   + 13*uo  -  5*up  +  1*upp )/12 + ...
     w3p.*(25*uo   - 23*up  + 13*upp -  3*uppp)/12;
end