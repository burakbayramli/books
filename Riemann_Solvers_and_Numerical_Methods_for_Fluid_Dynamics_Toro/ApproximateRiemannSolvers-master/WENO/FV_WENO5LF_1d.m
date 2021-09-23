function res = FV_WENO5LF_1d(q,smax,nx,dx)
% *************************************************************************
%
% Based on:
% C.W. Shu's Lectures notes on: 'ENO and WENO schemes for Hyperbolic
% Conservation Laws' 
%
% coded by Manuel A. Diaz, 02.10.2012, NTU Taiwan.
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

global gamma
R=3; I=R:(nx-R); % R: stencil size

%% Compute primitive variables at solution points
w(1,:) = q(1,:);
w(2,:) = q(2,:)./q(1,:);
w(3,:) = (gamma-1)*( q(3,:) - 0.5*(q(2,:).^2)./q(1,:));

%% Right State Extrapolation $u_{i+1/2}^{-}$
vmm = w(:,I-2);
vm  = w(:,I-1);
v   = w(:, I );
vp  = w(:,I+1);
vpp = w(:,I+2);

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

% Numerical Flux at cell boundary, $u_{i+1/2}^{-}$;
wn  = w0n.*(2*vmm - 7*vm + 11*v)/6 ...
    + w1n.*( -vm  + 5*v  + 2*vp)/6 ...
    + w2n.*(2*v   + 5*vp - vpp )/6;

%% Left State Extrapolation $u_{i+1/2}^{+}$ 
umm = w(:,I-1);
um  = w(:, I );
u   = w(:,I+1);
up  = w(:,I+2);
upp = w(:,I+3);

% Smooth Indicators (Beta factors)
B0p = 13/12*(umm-2*um+u  ).^2 + 1/4*(umm-4*um+3*u).^2; 
B1p = 13/12*(um -2*u +up ).^2 + 1/4*(um-up).^2;
B2p = 13/12*(u  -2*up+upp).^2 + 1/4*(3*u -4*up+upp).^2;

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

% Numerical Flux at cell boundary, $u_{i+1/2}^{+}$;
wp  = w0p.*( -umm + 5*um + 2*u  )/6 ...
	+ w1p.*( 2*um + 5*u  - up   )/6 ...
	+ w2p.*(11*u  - 7*up + 2*upp)/6;

%% Compute conservative variables at faces
qn(1,:) = wn(1,:);
qn(2,:) = wn(2,:).*wn(1,:);
qn(3,:) = wn(3,:)./(gamma-1) + 0.5*wn(1,:).*wn(2,:).^2;

qp(1,:) = wp(1,:);
qp(2,:) = wp(2,:).*wp(1,:);
qp(3,:) = wp(3,:)./(gamma-1) + 0.5*wp(1,:).*wp(2,:).^2;

%% Compute finite volume residual term, df/dx.
res=zeros(size(w)); flux=zeros(size(w));
for j = I % for all faces of the domain cells
    % compute flux at i+1/2
    flux(:,j) = LFflux(qn(:,j-2),qp(:,j-2),gamma,smax);
    % Flux contribution to the residual of every cell
    res(:, j ) = res(:, j ) + flux(:,j)/dx;
    res(:,j+1) = res(:,j+1) - flux(:,j)/dx;
end

% Flux contribution of the LEFT MOST FACE: left face of cell j=1.
flux(:,3) = LFflux(qn(:,R),qp(:,R),gamma,smax);
res(:,3) = res(:,3) - flux(:,3)/dx;
 
% Flux contribution of the RIGHT MOST FACE: right face of cell j=nx-1.
flux(:,nx-2) = LFflux(qn(:,nx-1-2*R),qp(:,nx-1-2*R),gamma,smax);
res(:,nx-2) = res(:,nx-2) + flux(:,nx-2)/dx;

end % FVM WENO

function FL = LFflux(qL,qR,gamma,smax)
    % Lax-Friedrichs flux:
    %
    % P. D. Lax, Weak Solutions of Nonlinear Hyperbolic Equations and Their
    % Numerical Computation, Commun. Pure and Applied Mathematics, 7, 159-193, 
    % 1954.
    
    % Left state
    rL = qL(1);
    uL = qL(2)./rL;
    pL = (gamma-1)*( qL(3) - rL*uL*uL/2 );
    HL = ( qL(3) + pL ) / rL;
    
    % Right state
    rR = qR(1);
    uR = qR(2)./rR;
    pR = (gamma-1)*( qR(3) - rR*uR*uR/2 );
    HR = ( qL(3) + pL ) / rL;
    
    FL=[rL*uL; rL*uL^2+pL; uL*(rL*HL)];
    FR=[rR*uR; rR*uR^2+pR; uR*(rR*HR)];
    
    % Lax-Friedrichs Numerical Flux
    FL = 0.5*( FR + FL + smax*(qL-qR) );
end