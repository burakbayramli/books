function res = FD_WENO_EE2d(q,a,nx,ny,dx,dy,t,fsplitMth,Recon,Test)
% Compute RHS of the semi-discrete form of the Euler equations.
global preshock postshock mesh_wedge_position

%   Flux at j+1/2
% 
%     j+1/2    Cell's grid: (assuming WENO5, R=3)
%   |   |   |                   {x=0}             {x=L}
%   | wL|   |                     |                 |
%   |  /|wR |           1   2   3 | 4   5        N-3|N-2 N-1  N
%   | / |\  |         |-o-|-o-|-o-|-o-|-o-| ... |-o-|-o-|-o-|-o-|---> j
%   |/  | \ |             1   2   3   4   6    N-4 N-3 N-2 N-1  
%   |   |  \|                    {1} {2} {3}  ...  {nf}
%   |   |   |       NC: Here cells 1 to 3 and N-2 to N are ghost cells
%     j  j+1            faces 3 and N-3, are the real boundary faces.
%
%   q = cat(3, r, ru, rv, E);
%   F = cat(3, ru, ru^2+p, ruv, u(E+p));
%   G = cat(3, rv, ruv, rv^2+p, v(E+p));

% 1. Set boundary conditions 

    % Identify number of gost cells
    switch Recon
        case {'WENO5','Poly5'}, R=3; % R: stencil size and number of gost cells
        case {'WENO7','Poly7'}, R=4;
        otherwise, error('reconstruction not available ;P');
    end

    % Set boundary conditions on ghost cells
    switch Test
        case 'Smooth' % Set Periodic BCs
            for i=1:R
                q(:,i,:)=q(:,nx-R+i,:); q(:,nx-2*R+i,:)=q(:,R+i,:);	% Periodic BCs
            end
            for j=1:R
                q(j,:,:)=q(ny-R+i,:,:); q(ny-2*R+j,:,:)=q(R+j,:,:);	% Periodic BCs
            end
        case 'Riemann' % Set outflow BCs
            for i=1:R
                q(:,i,:)=q(:,R+1,:); q(:,nx+1-i,:)=q(:,nx-R,:);	% Neumann BCs
            end
            for j=1:R
                q(j,:,:)=q(R+1,:,:); q(ny+1-j,:,:)=q(ny-R,:,:);	% Neumann BCs
            end
        case 'DMR' % Set DMR test BCs
            % Static BCs
            for i=1:R
                q(:,i,:)=q(:,R+1,:); q(:,nx+1-i,:)=q(:,nx-R,:);	% Neumann BCs
            end
            % Static BCs at the bottom of domain
            for j=1:R
                for i=R+1:nx-R
                    if i<(R+mesh_wedge_position)
                        q(j,i,:)=q(R+1,i,:); % outflow condition : Neumann BC
                    else
                        q(j,i,:)=q(R+1,i,:); q(j,i,3)=-q(R+1,i,3); % EE reflective BC
                    end
                end
            end
            % Time dependent BCs at the top of domain: moving shock
            for j=ny+1-R:ny % only gosht cells at the top
                for i=R+1:nx-R % evaluate all x domain
                    if distance_to_shock(i*dx+dx/2,(j+R)*dy+dy/2,t) < -3*dx % mesh_shock
                        q(j,i,:)=q(ny-R,i,:); % Neumann BC
                    elseif distance_to_shock(i*dx+dx/2,(j+R)*dy+dy/2,t) > 3*dx % mesh_shock
                        q(j,i,:)=q(ny-R,i,:); % Neumann BC
                    elseif distance_to_shock(i*dx+dx/2,(j+R)*dy+dy/2,t) < 0 % mesh_shock
                        q(j,i,:)=postshock; % Dirichlet BCs
                    else
                        q(j,i,:)=preshock; % Dirichlet BCs
                    end
                end
            end
        otherwise, error('Test boundaries not set!');
    end

% 2. Produce flux splitting in x-direction

    % we only consider internal cells 
    ic=R+1:ny-R;  

    % Normal unitary face vectors: (nx,ny)
    % normals = {[1,0], [0,1]}; % i.e.: x-axis, y-axis
    switch fsplitMth
        case 'LF',  [fp,fm] = LF(a,q(ic,:,:),[1,0]);    % Lax-Friedrichs (LF) Flux Splitting
        case 'LLF', [fp,fm] = Rusanov(q(ic,:,:),[1,0]); % Local Lax-Friedrichs (LF) Flux Splitting
        otherwise, error('Splitting method not set.');
    end

% 3. Reconstruct interface values: qL=q_{i+1/2}^{-} and qR=q_{i-1/2}^{+}
    E=4; % numer of components or layers
    parfor e=1:E
        switch Recon
            case 'WENO5', [flux(e,:)] = WENO5recon_X(fp(:,:,e),fm(:,:,e),nx);
            case 'WENO7', [flux(e,:)] = WENO7recon_X(fp(:,:,e),fm(:,:,e),nx);
            case 'Poly5', [flux(e,:)] = POLY5recon_X(fp(:,:,e),fm(:,:,e),nx);
            case 'Poly7', [flux(e,:)] = POLY7recon_X(fp(:,:,e),fm(:,:,e),nx);
            otherwise, error('reconstruction not available ;P');
        end
    end

% 4. Compute finite volume residual term, df/dx.
    res=zeros(size(q)); nc=ny-2*R; nf=nx+1-2*R;

    % Flux contribution to the residual of every cell
    for e=1:E
        for j=1:nc % for all interior cells
            res(j+R,R+1,e) = res(j+R,R+1,e) - flux(e,j+nc*(1-1))/dx; % left face of cell j=4.
            for i = 2:nf-1 % for all interior faces
                res(j+R,i+R-1,e) = res(j+R,i+R-1,e) + flux(e,j+nc*(i-1))/dx;
                res(j+R, i+R ,e) = res(j+R, i+R ,e) - flux(e,j+nc*(i-1))/dx;
            end
            res(j+R,nx-R,e) = res(j+R,nx-R,e) + flux(e,j+nc*(nf-1))/dx; % right face of cell j=N-3.
        end
    end

    % Clear flux variables
    clear flux fp fm;
    
% 5. Produce flux splitting in y-direction

    % we only consider internal cells
    ic=R+1:nx-R;
    switch fsplitMth
        case 'LF',  [fp,fm] = LF(a,q(:,ic,:),[0,1]);    % Lax-Friedrichs (LF) Flux Splitting
        case 'LLF', [fp,fm] = Rusanov(q(:,ic,:),[0,1]); % Local Lax-Friedrichs (LF) Flux Splitting
        otherwise, error('Splitting method not set.');
    end

% 6. Reconstruct interface values: qL=q_{j+1/2}^{-} and qR=q_{j-1/2}^{+}
    parfor e=1:E
        switch Recon
            case 'WENO5', [flux(e,:)] = WENO5recon_Y(fp(:,:,e),fm(:,:,e),ny);
            case 'WENO7', [flux(e,:)] = WENO7recon_Y(fp(:,:,e),fm(:,:,e),ny);
            case 'Poly5', [flux(e,:)] = POLY5recon_Y(fp(:,:,e),fm(:,:,e),ny);
            case 'Poly7', [flux(e,:)] = POLY7recon_Y(fp(:,:,e),fm(:,:,e),ny);
        end
    end

% 7. Flux contribution to the residual of every cell
    nc=nx-2*R; nf=ny+1-2*R;
    for e=1:E
        for i=1:nc % for all interior cells
            res(R+1,i+R,e) = res(R+1,i+R,e) - flux(e,1+nf*(i-1))/dy;
            for j=2:nf-1 % for all interior cells
                res(j+R-1,i+R,e) = res(j+R-1,i+R,e) + flux(e,j+nf*(i-1))/dy;
                res( j+R ,i+R,e) = res( j+R ,i+R,e) - flux(e,j+nf*(i-1))/dy;
            end
            res(ny-R,i+R,e) = res(ny-R,i+R,e) + flux(e,nf+nf*(i-1))/dy;
        end
    end

end % FVM WENO

%%%%%%%%%%%%%%%%%%%
% Distance to shock (for Double Mach Reflection)
%%%%%%%%%%%%%%%%%%%

function distance = distance_to_shock(x,y,t)
    global shock_speed
    shock_slope = 1/tan(pi/6); % from problem definition
    wedge_position = 1/6; % from problem definition
    distance = (shock_slope*(x-wedge_position-shock_speed*t)-y) / sqrt((shock_slope)^2+1);
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Flux Splitting functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Lax-Friedrichs
function [Fp,Fm] = LF(a,q,normal)
    global gamma
    
    % Normal vectors
    nx = normal(1);
    ny = normal(2);
    
    % primary properties
    r=q(:,:,1); u=q(:,:,2)./r; v=q(:,:,3)./r; E=q(:,:,4); vn=u*nx+v*ny;
    p=(gamma-1)*(E-0.5*r.*(u.^2+v.^2));
    
    % Flux vector of conserved properties
    % F=[r.*u; r.*u.^2+p; r.*u.*v;   u.*(E+p)];
    % G=[r.*v; r.*u.*v;   r.*v.^2+p; v.*(E+p)];
    F=cat(3, r.*vn, r.*vn.*u + p*nx, r.*vn.*v + p*ny, vn.*(E+p));
    
    % Lax-Friedrichs flux
    Fp=0.5*(F + a*q); 
    Fm=0.5*(F - a*q); 
end

% Rusanov (or local Lax-Friedrichs)
function [Fp,Fm] = Rusanov(q,normal)
    global gamma
    
    % Normal vectors
    nx = normal(1);
    ny = normal(2);
    
    % primary properties
    r=q(:,:,1); u=q(:,:,2)./r; v=q(:,:,3)./r; E=q(:,:,4); vn=u*nx+v*ny;
    p=(gamma-1)*(E-0.5*r.*(u.^2+v.^2)); a=sqrt(gamma*p./r); 
    
    % Flux vector of conserved properties
    % F=[r.*u; r.*u.^2+p; r.*u.*v;   u.*(E+p)];
    % G=[r.*v; r.*u.*v;   r.*v.^2+p; v.*(E+p)];
    F=cat(3, r.*vn, r.*vn.*u + p*nx, r.*vn.*v + p*ny, vn.*(E+p));
    
    % positive and negative fluxes
    I=ones(3,1); % I = [1;1;1;] column vector
    Fp=0.5*(F + I*a.*q); 
    Fm=0.5*(F - I*a.*q); 
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% WENO and Polynomail reconstructions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [flux] = WENO5recon_X(v,u,N)
% *************************************************************************
% Based on:
% Shu, Chi-Wang. "High order weighted essentially nonoscillatory schemes
% for convection dominated problems." SIAM review 51.1 (2009): 82-126.  
%
% coded by Manuel Diaz, 2016.04.29, NHRI Taiwan.
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
I=3:(N-3); % R:3 stencil size

%% Left State Extrapolation $u_{i+1/2}^{-}$
vmm = reshape(v(:,I-2,:),1,[]);
vm  = reshape(v(:,I-1,:),1,[]);
vo  = reshape(v(:, I ,:),1,[]);
vp  = reshape(v(:,I+1,:),1,[]);
vpp = reshape(v(:,I+2,:),1,[]);

% Smooth Indicators (Beta factors)
B0n = 13/12*(vmm-2*vm+vo ).^2 + 1/4*(vmm-4*vm+3*vo).^2; 
B1n = 13/12*(vm -2*vo+vp ).^2 + 1/4*(vm-vp).^2;
B2n = 13/12*(vo -2*vp+vpp).^2 + 1/4*(3*vo-4*vp+vpp).^2;

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
flux = w0n.*(2*vmm - 7*vm + 11*vo)/6 ...
     + w1n.*( -vm  + 5*vo  + 2*vp)/6 ...
     + w2n.*(2*vo   + 5*vp - vpp )/6;

%% Right State Extrapolation $u_{i+1/2}^{+}$ 
umm = reshape(u(:,I-1,:),1,[]);
um  = reshape(u(:, I ,:),1,[]);
uo  = reshape(u(:,I+1,:),1,[]);
up  = reshape(u(:,I+2,:),1,[]);
upp = reshape(u(:,I+3,:),1,[]);

% Smooth Indicators (Beta factors)
B0p = 13/12*(umm-2*um+uo ).^2 + 1/4*(umm-4*um+3*uo).^2; 
B1p = 13/12*(um -2*uo+up ).^2 + 1/4*(um-up).^2;
B2p = 13/12*(uo -2*up+upp).^2 + 1/4*(3*uo -4*up+upp).^2;

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
flux = flux + w0p.*( -umm + 5*um + 2*uo  )/6 ...
	        + w1p.*( 2*um + 5*uo  - up   )/6 ...
	        + w2p.*(11*uo  - 7*up + 2*upp)/6;
end

function [flux] = WENO5recon_Y(v,u,N)
% *************************************************************************
% Input: u(i) = [u(i-2) u(i-1) u(i) u(i+1) u(i+2)];
% Output: res = df/dx;
%
% Based on:
% Shu, Chi-Wang. "High order weighted essentially nonoscillatory schemes
% for convection dominated problems." SIAM review 51.1 (2009): 82-126.  
%
% coded by Manuel Diaz, 2016.04.29, NHRI Taiwan.
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
I=3:(N-3); % R: stencil size

%% Left State Extrapolation $u_{i+1/2}^{-}$
vmm = reshape(v(I-2,:,:),1,[]);
vm  = reshape(v(I-1,:,:),1,[]);
vo  = reshape(v( I ,:,:),1,[]);
vp  = reshape(v(I+1,:,:),1,[]);
vpp = reshape(v(I+2,:,:),1,[]);

% Smooth Indicators (Beta factors)
B0n = 13/12*(vmm-2*vm+vo  ).^2 + 1/4*(vmm-4*vm+3*vo).^2; 
B1n = 13/12*(vm -2*vo +vp ).^2 + 1/4*(vm-vp).^2;
B2n = 13/12*(vo  -2*vp+vpp).^2 + 1/4*(3*vo-4*vp+vpp).^2;

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
flux = w0n.*(2*vmm - 7*vm + 11*vo)/6 ...
     + w1n.*( -vm  + 5*vo  + 2*vp)/6 ...
     + w2n.*(2*vo   + 5*vp - vpp )/6;

%% Right State Extrapolation $u_{i+1/2}^{+}$ 
umm = reshape(u(I-1,:,:),1,[]);
um  = reshape(u( I ,:,:),1,[]);
uo  = reshape(u(I+1,:,:),1,[]);
up  = reshape(u(I+2,:,:),1,[]);
upp = reshape(u(I+3,:,:),1,[]);

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

% Numerical Flux at cell boundary, $u_{i+1/2}^{+}$;
flux = flux + w0p.*( -umm + 5*um + 2*uo  )/6 ...
	        + w1p.*( 2*um + 5*uo  - up   )/6 ...
	        + w2p.*(11*uo  - 7*up + 2*upp)/6;
end

function [flux] = POLY5recon_X(v,u,N)
% Direct Polynomial reconstruction
I=3:(N-3); % R:3 stencil size

%% Left Flux: f_{i+1/2}^{-}
vmm = reshape(v(:,I-2,:),1,[]);
vm  = reshape(v(:,I-1,:),1,[]);
vo  = reshape(v(:, I ,:),1,[]);
vp  = reshape(v(:,I+1,:),1,[]);
vpp = reshape(v(:,I+2,:),1,[]);

% Numerical Flux at cell boundary, $u_{i+1/2}^{-}$;
flux = ( 2*vmm - 13*vm + 47*vo + 27*vp - 3*vpp)/60;

%% Right Flux: f_{i+1/2}^{+}
umm = reshape(u(:,I-1,:),1,[]);
um  = reshape(u(:, I ,:),1,[]);
uo  = reshape(u(:,I+1,:),1,[]);
up  = reshape(u(:,I+2,:),1,[]);
upp = reshape(u(:,I+3,:),1,[]);

% Numerical Flux at cell boundary, $u_{i+1/2}^{+}$;
flux = flux + (-3*umm + 27*um + 47*uo - 13*up + 2*upp)/60;
end

function [flux] = POLY5recon_Y(v,u,N)
% The stencil size
I=3:(N-3); 

%% Left Flux: f_{i+1/2}^{-}
vmm = reshape(v(I-2,:,:),1,[]);
vm  = reshape(v(I-1,:,:),1,[]);
vo  = reshape(v( I ,:,:),1,[]);
vp  = reshape(v(I+1,:,:),1,[]);
vpp = reshape(v(I+2,:,:),1,[]);

% Numerical Flux at cell boundary, $u_{i+1/2}^{-}$;
flux = ( 2*vmm - 13*vm + 47*vo + 27*vp - 3*vpp)/60;

%% Right Flux: f_{i+1/2}^{+}
umm = reshape(u(I-1,:,:),1,[]);
um  = reshape(u( I ,:,:),1,[]);
uo  = reshape(u(I+1,:,:),1,[]);
up  = reshape(u(I+2,:,:),1,[]);
upp = reshape(u(I+3,:,:),1,[]);

% Numerical Flux at cell boundary, $u_{i+1/2}^{+}$;
flux = flux + (-3*umm + 27*um + 47*uo - 13*up + 2*upp)/60;
end

function [flux] = WENO7recon_X(v,u,N)
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
% The stencil size
I=4:(N-4); 

%% Right Flux: f_{i+1/2}^{-}
vmmm= reshape(v(:,I-3,:),1,[]);
vmm = reshape(v(:,I-2,:),1,[]);
vm  = reshape(v(:,I-1,:),1,[]);
vo  = reshape(v(:, I ,:),1,[]);
vp  = reshape(v(:,I+1,:),1,[]);
vpp = reshape(v(:,I+2,:),1,[]);
vppp= reshape(v(:,I+3,:),1,[]);

% Smooth Indicators
B0n = vm.*(134241*vm-114894*vo)   +vmmm.*(56694*vm-47214*vmm+6649*vmmm-22778*vo)...
        +25729*vo.^2  +vmm.*(-210282*vm+85641*vmm+86214*vo);
B1n = vo.*(41001*vo-30414*vp)     +vmm.*(-19374*vm+3169*vmm+19014*vo-5978*vp)...
        +6649*vp.^2   +vm.*(33441*vm-70602*vo+23094*vp);
B2n = vp.*(33441*vp-19374*vpp)    +vm.*(6649*vm-30414*vo+23094*vp-5978*vpp)...
        +3169*vpp.^2  +vo.*(41001*vo-70602*vp+19014*vpp);
B3n = vpp.*(85641*vpp-47214*vppp) +vo.*(25729*vo-114894*vp+86214*vpp-22778*vppp)...
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

% Numerical Flux at cell boundary, $u_{i+1/2}^{-}$;
flux = w0n.*(-3*vmmm + 13*vmm - 23*vm  + 25*vo  )/12 ...
     + w1n.*( 1*vmm  - 5*vm   + 13*vo  +  3*vp  )/12 ...
     + w2n.*(-1*vm   + 7*vo   +  7*vp  -  1*vpp )/12 ...
     + w3n.*( 3*vo   + 13*vp  -  5*vpp +  1*vppp)/12;

%% Left Flux: f_{i+1/2}^{+}
ummm= reshape(u(:,I-2,:),1,[]);
umm = reshape(u(:,I-1,:),1,[]);
um  = reshape(u(:, I ,:),1,[]);
uo  = reshape(u(:,I+1,:),1,[]);
up  = reshape(u(:,I+2,:),1,[]);
upp = reshape(u(:,I+3,:),1,[]);
uppp= reshape(u(:,I+4,:),1,[]);

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
flux = flux + w0p.*( 1*ummm - 5*umm  + 13*um  +  3*uo  )/12 ...
            + w1p.*(-1*umm  + 7*um   +  7*uo  -  1*up  )/12 ... 
            + w2p.*( 3*um   + 13*uo  -  5*up  +  1*upp )/12 ...
            + w3p.*(25*uo   - 23*up  + 13*upp -  3*uppp)/12;
end


function [flux] = WENO7recon_Y(v,u,N)
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
% The stencil size
I=4:(N-4); 

%% Right Flux: f_{i+1/2}^{-}
vmmm= reshape(v(I-3,:,:),1,[]);
vmm = reshape(v(I-2,:,:),1,[]);
vm  = reshape(v(I-1,:,:),1,[]);
vo  = reshape(v( I ,:,:),1,[]);
vp  = reshape(v(I+1,:,:),1,[]);
vpp = reshape(v(I+2,:,:),1,[]);
vppp= reshape(v(I+3,:,:),1,[]);

% Smooth Indicators
B0n = vm.*(134241*vm-114894*vo)   +vmmm.*(56694*vm-47214*vmm+6649*vmmm-22778*vo)...
        +25729*vo.^2  +vmm.*(-210282*vm+85641*vmm+86214*vo);
B1n = vo.*(41001*vo-30414*vp)     +vmm.*(-19374*vm+3169*vmm+19014*vo-5978*vp)...
        +6649*vp.^2   +vm.*(33441*vm-70602*vo+23094*vp);
B2n = vp.*(33441*vp-19374*vpp)    +vm.*(6649*vm-30414*vo+23094*vp-5978*vpp)...
        +3169*vpp.^2  +vo.*(41001*vo-70602*vp+19014*vpp);
B3n = vpp.*(85641*vpp-47214*vppp) +vo.*(25729*vo-114894*vp+86214*vpp-22778*vppp)...
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

% Numerical Flux at cell boundary, $u_{i+1/2}^{-}$;
flux = w0n.*(-3*vmmm + 13*vmm - 23*vm  + 25*vo  )/12 ...
     + w1n.*( 1*vmm  - 5*vm   + 13*vo  +  3*vp  )/12 ...
     + w2n.*(-1*vm   + 7*vo   +  7*vp  -  1*vpp )/12 ...
     + w3n.*( 3*vo   + 13*vp  -  5*vpp +  1*vppp)/12;

%% Left Flux: f_{i+1/2}^{+}
ummm= reshape(u(I-2,:,:),1,[]);
umm = reshape(u(I-1,:,:),1,[]);
um  = reshape(u( I ,:,:),1,[]);
uo  = reshape(u(I+1,:,:),1,[]);
up  = reshape(u(I+2,:,:),1,[]);
upp = reshape(u(I+3,:,:),1,[]);
uppp= reshape(u(I+4,:,:),1,[]);

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
flux = flux + w0p.*( 1*ummm - 5*umm  + 13*um  +  3*uo  )/12 ...
            + w1p.*(-1*umm  + 7*um   +  7*uo  -  1*up  )/12 ... 
            + w2p.*( 3*um   + 13*uo  -  5*up  +  1*upp )/12 ...
            + w3p.*(25*uo   - 23*up  + 13*upp -  3*uppp)/12;
end

function [flux] = POLY7recon_X(v,u,N)
% The stencil size
I=4:(N-4);  

%% Left Flux: f_{i+1/2}^{-}
vmmm= reshape(v(:,I-3,:),1,[]);
vmm = reshape(v(:,I-2,:),1,[]);
vm  = reshape(v(:,I-1,:),1,[]);
vo  = reshape(v(:, I ,:),1,[]);
vp  = reshape(v(:,I+1,:),1,[]);
vpp = reshape(v(:,I+2,:),1,[]);
vppp= reshape(v(:,I+3,:),1,[]);

% Numerical Flux at cell boundary, $u_{i+1/2}^{-}$;
flux = (-3*vmmm + 25*vmm - 101*vm  + 319*vo + 214*vp - 38*vpp + 4*vppp)/420;

%% Right Flux: f_{i+1/2}^{+}
ummm= reshape(u(:,I-2,:),1,[]);
umm = reshape(u(:,I-1,:),1,[]);
um  = reshape(u(:, I ,:),1,[]);
uo  = reshape(u(:,I+1,:),1,[]);
up  = reshape(u(:,I+2,:),1,[]);
upp = reshape(u(:,I+3,:),1,[]);
uppp= reshape(u(:,I+4,:),1,[]);

% Numerical Flux at cell boundary, $u_{i+1/2}^{+}$;
flux = flux + (4*ummm - 38*umm  + 214*um  + 319*uo - 101*up + 25*upp - 3*uppp)/420;
end

function [flux] = POLY7recon_Y(v,u,N)
% The stencil size
I=4:(N-4);  

%% Left Flux: f_{i+1/2}^{-}
vmmm= reshape(v(I-3,:,:),1,[]);
vmm = reshape(v(I-2,:,:),1,[]);
vm  = reshape(v(I-1,:,:),1,[]);
vo  = reshape(v( I ,:,:),1,[]);
vp  = reshape(v(I+1,:,:),1,[]);
vpp = reshape(v(I+2,:,:),1,[]);
vppp= reshape(v(I+3,:,:),1,[]);

% Numerical Flux at cell boundary, $u_{i+1/2}^{-}$;
flux = (-3*vmmm + 25*vmm - 101*vm  + 319*vo + 214*vp - 38*vpp + 4*vppp)/420;

%% Right Flux: f_{i+1/2}^{+}
ummm= reshape(u(I-2,:,:),1,[]);
umm = reshape(u(I-1,:,:),1,[]);
um  = reshape(u( I ,:,:),1,[]);
uo  = reshape(u(I+1,:,:),1,[]);
up  = reshape(u(I+2,:,:),1,[]);
upp = reshape(u(I+3,:,:),1,[]);
uppp= reshape(u(I+4,:,:),1,[]);

% Numerical Flux at cell boundary, $u_{i+1/2}^{+}$;
flux = flux + (4*ummm - 38*umm  + 214*um  + 319*uo - 101*up + 25*upp - 3*uppp)/420;
end