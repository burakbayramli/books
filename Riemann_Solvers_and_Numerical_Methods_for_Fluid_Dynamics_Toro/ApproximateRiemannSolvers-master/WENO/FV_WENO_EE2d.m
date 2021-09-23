function res = FV_WENO_EE2d(q,a,nx,ny,dx,dy,t,fluxMethod,Recon,Test)
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

% Identify number of gost cells
switch Recon
    case {'WENO5','Poly5'}, R=3; % R: stencil size and number of gost cells
    case {'WENO7','Poly7'}, R=4;
    otherwise, error('reconstruction not available ;P');
end

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

% internal cells and primitives array
ic=R+1:ny-R;  %w=zeros(numel(ic),nx,4);

% 1. Reconstruct interface values: qL=q_{i+1/2}^{-} and qR=q_{i-1/2}^{+}
E=4; % numer of components or layers
parfor e=1:E
    switch Recon
        case 'WENO5', [qL(e,:),qR(e,:)] = WENO5recon_X(q(ic,:,e),nx);
        case 'WENO7', [qL(e,:),qR(e,:)] = WENO7recon_X(q(ic,:,e),nx);
        case 'Poly5', [qL(e,:),qR(e,:)] = POLY5recon_X(q(ic,:,e),nx);
        case 'Poly7', [qL(e,:),qR(e,:)] = POLY7recon_X(q(ic,:,e),nx);
        otherwise, error('reconstruction not available ;P');
    end
end

% 2. Compute finite volume residual term, df/dx.
res=zeros(size(q)); flux=zeros(size(qR)); nc=ny-2*R; nf=nx+1-2*R; N=nc*nf;

% Normal unitary face vectors: (nx,ny)
% normals = {[1,0], [0,1]}; % i.e.: x-axis, y-axis

% compute flux at (i+1/2,j)
for c=1:N
    switch fluxMethod
        case 'LF',  flux(:,c) = LFflux(qL(:,c),qR(:,c),[1,0],a); % Lax Friedrichs
        case 'ROE', flux(:,c) = ROEflux(qL(:,c),qR(:,c),[1,0]);  % Roe
        case 'LLF', flux(:,c) = RUSflux(qL(:,c),qR(:,c),[1,0]);  % Rusanov
        case 'HLLE',flux(:,c) = HLLEflux(qL(:,c),qR(:,c),[1,0]); % HLLE
        case 'HLLC',flux(:,c) = HLLCflux(qL(:,c),qR(:,c),[1,0]); % HLLC
        otherwise, error('flux method not available ;P');
    end
end

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

% Compute primitive variables at solution points (they still in memory)
clear qL qR wL wR flux;

% internal cells and primitives array
ic=R+1:nx-R;  %w=zeros(ny,numel(ic),4);

% 1. Reconstruct interface values: qL=q_{j+1/2}^{-} and qR=q_{j-1/2}^{+}
parfor e=1:E
    switch Recon
        case 'WENO5', [qL(e,:),qR(e,:)] = WENO5recon_Y(q(:,ic,e),ny);
        case 'WENO7', [qL(e,:),qR(e,:)] = WENO7recon_Y(q(:,ic,e),ny);
        case 'Poly5', [qL(e,:),qR(e,:)] = POLY5recon_Y(q(:,ic,e),ny);
        case 'Poly7', [qL(e,:),qR(e,:)] = POLY7recon_Y(q(:,ic,e),ny);
    end
end

% 2. Compute finite volume residual term, df/dx.
flux=zeros(size(qL)); nc=nx-2*R; nf=ny+1-2*R; N=nc*nf;

% compute flux at (i,j+1/2)
for c=1:N
    switch fluxMethod
        case 'LF',  flux(:,c) = LFflux(qL(:,c),qR(:,c),[0,1],a); % Lax Friedrichs
        case 'ROE', flux(:,c) = ROEflux(qL(:,c),qR(:,c),[0,1]);  % Roe
        case 'LLF', flux(:,c) = RUSflux(qL(:,c),qR(:,c),[0,1]);  % Rusanov
        case 'HLLE',flux(:,c) = HLLEflux(qL(:,c),qR(:,c),[0,1]); % HLLE
        case 'HLLC',flux(:,c) = HLLCflux(qL(:,c),qR(:,c),[0,1]); % HLLC
    end
end

% Flux contribution to the residual of every cell
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

%%%%%%%%%%%%%%%%%
% Flux functions
%%%%%%%%%%%%%%%%%

function LF = LFflux(qL,qR,normal,smax)
    % Lax-Friedrichs flux
    global gamma

    % Normal vectors
    nx = normal(1);
    ny = normal(2);
    
    % Left state
    rL = qL(1);
    uL = qL(2)/rL;
    vL = qL(3)/rL;
    vnL= uL*nx + vL*ny;
    pL = (gamma-1)*( qL(4) - 0.5*rL*(uL^2+vL^2) );
    HL = ( qL(4) + pL ) / rL;
    
    % Right state
    rR = qR(1);
    uR = qR(2)/rR;
    vR = qR(3)/rR;
    vnR= uR*nx + vR*ny;
    pR = (gamma-1)*( qR(4) - 0.5*rR*(uR^2+vR^2) );
    HR = ( qR(4) + pR ) / rR;
    
    % Left and Right fluxes
    FL=[rL*vnL; rL*vnL*uL + pL*nx; rL*vnL*vL + pL*ny; rL*vnL*HL];
    FR=[rR*vnR; rR*vnR*uR + pR*nx; rR*vnR*vR + pR*ny; rR*vnR*HR];
    
    % Rusanov numerical flux
    LF = 0.5*( FR + FL + smax*(qL-qR) );
end

function Rusanov = RUSflux(qL,qR,normal)
    % Rusanov flux 
    global gamma
    
    % Normal vectors
    nx = normal(1);
    ny = normal(2);
    
    % Left state
    rL = qL(1);
    uL = qL(2)/rL;
    vL = qL(3)/rL;
    vnL= uL*nx + vL*ny;
    pL = (gamma-1)*( qL(4) - 0.5*rL*(uL^2+vL^2) );
    HL = ( qL(4) + pL ) / rL;

    % Right state
    rR = qR(1);
    uR = qR(2)/rR;
    vR = qR(3)/rR;
    vnR= uR*nx + vR*ny;
    pR = (gamma-1)*( qR(4) - 0.5*rR*(uR^2+vR^2) );
    HR = ( qR(4) + pR ) / rR;
    
    % First compute the Roe Averages
    RT = sqrt(rR/rL);
    %r= RT*rL;
    u = (uL+RT*uR)/(1+RT);
    v = (vL+RT*vR)/(1+RT);
    H = ( HL+RT* HR)/(1+RT);
    a = sqrt( (gamma-1)*(H-(u^2+v^2)/2) );
    
    % Left and Right fluxes
    FL=[rL*vnL; rL*vnL*uL + pL*nx; rL*vnL*vL + pL*ny; rL*vnL*HL];
    FR=[rR*vnR; rR*vnR*uR + pR*nx; rR*vnR*vR + pR*ny; rR*vnR*HR];
    
    % Rusanov numerical flux
    smax = abs(sqrt(u^2+v^2))+a; Rusanov = 0.5*( FR + FL + smax*(qL-qR) );
end

function Roe = ROEflux(qL,qR,normal)
    % Compute Roe flux
    global gamma
    
    % normal vectors
    nx = normal(1);
    ny = normal(2);
    
    % Tangent vectors
    tx = -ny;
    ty = nx;
    
    % Left state
    rL = qL(1);
    uL = qL(2)/rL;
    vL = qL(3)/rL;
    vnL = uL*nx+vL*ny;
    vtL = uL*tx+vL*ty;
    pL = (gamma-1)*( qL(4) - rL*(uL^2+vL^2)/2 );
    %aL = sqrt(gamma*pL/rL);
    HL = ( qL(4) + pL ) / rL;
    
    % Right state
    rR = qR(1);
    uR = qR(2)/rR;
    vR = qR(3)/rR;
    vnR = uR*nx+vR*ny;
    vtR = uR*tx+vR*ty;
    pR = (gamma-1)*( qR(4) - rR*(uR^2+vR^2)/2 );
    %aR = sqrt(gamma*pR/rR);
    HR = ( qR(4) + pR ) / rR;
    
    % First compute the Roe Averages
    RT = sqrt(rR/rL);
    r = RT*rL;
    u = (uL+RT*uR)/(1+RT);
    v = (vL+RT*vR)/(1+RT);
    H = ( HL+RT* HR)/(1+RT);
    a = sqrt( (gamma-1)*(H-(u^2+v^2)/2) );
    vn = u*nx+v*ny;
    vt = u*tx+v*ty;
    
    % Wave Strengths
    dr = rR - rL;     dp = pR - pL;     dvn= vnR - vnL;     dvt= vtR - vtL;
    dV = [(dp-r*a*dvn )/(2*a^2); r*dvt/a; dr-dp/(a^2); (dp+r*a*dvn)/(2*a^2)];
    
    % Wave Speed
    ws = [abs(vn-a); abs(vn); abs(vn); abs(vn+a)];
    
    % Harten's Entropy Fix JCP(1983), 49, pp357-393:
    % only for the nonlinear fields.
    dws(1)=1/5; if ws(1)<dws(1); ws(1)=( ws(1)*ws(1)/dws(1)+dws(1) )/2; end
    dws(4)=1/5; if ws(4)<dws(4); ws(4)=( ws(4)*ws(4)/dws(4)+dws(4) )/2; end
    
    % Right Eigenvectors       
    Rv = [  1   ,  0   ,    1      ,  1   ;
          u-a*nx, a*tx ,    u      ,u+a*nx;
          u-a*ny, a*ty ,    u      ,u+a*ny;
          H-vn*a, vt*a ,(u^2+v^2)/2,H+vn*a];
    
    % Left and Right fluxes
    FL=[rL*vnL; rL*vnL*uL + pL*nx; rL*vnL*vL + pL*ny; rL*vnL*HL];
    FR=[rR*vnR; rR*vnR*uR + pR*nx; rR*vnR*vR + pR*ny; rR*vnR*HR];
    
    % Dissipation Term
    Roe = (FL + FR - Rv*(ws.*dV))/2;

end

function HLLE = HLLEflux(qL,qR,normal)
    % Compute HLLE flux
    global gamma

    % normal vectors
    nx = normal(1);
    ny = normal(2);
       
    % Left state
    rL = qL(1);
    uL = qL(2)/rL;
    vL = qL(3)/rL;
    vnL = uL*nx+vL*ny;
    pL = (gamma-1)*( qL(4) - rL*(uL^2+vL^2)/2 );
    aL = sqrt(gamma*pL/rL);
    HL = ( qL(4) + pL ) / rL;
    
    % Right state
    rR = qR(1);
    uR = qR(2)/rR;
    vR = qR(3)/rR;
    vnR = uR*nx+vR*ny;
    pR = (gamma-1)*( qR(4) - rR*(uR^2+vR^2)/2 );
    aR = sqrt(gamma*pR/rR);
    HR = ( qR(4) + pR ) / rR;
    
    % First compute the Roe Averages
    RT = sqrt(rR/rL); % r = RT*rL;
    u = (uL+RT*uR)/(1+RT);
    v = (vL+RT*vR)/(1+RT);
    H = ( HL+RT* HR)/(1+RT);
    a = sqrt( (gamma-1)*(H-(u^2+v^2)/2) );
    vn = u*nx+v*ny;
    
    % Wave speed estimates
    SLm = min([ vnL-aL, vn-a, 0]);
    SRp = max([ vnR+aR, vn+a, 0]);
    
    % Left and Right fluxes
    FL=[rL*vnL; rL*vnL*uL + pL*nx; rL*vnL*vL + pL*ny; rL*vnL*HL];
    FR=[rR*vnR; rR*vnR*uR + pR*nx; rR*vnR*vR + pR*ny; rR*vnR*HR];
    
    % Compute the HLL flux.
    HLLE = ( SRp*FL - SLm*FR + SLm*SRp*(qR-qL) )/(SRp-SLm);
end

function HLLC = HLLCflux(qL,qR,normal)
    % Compute HLLC flux
    global gamma

    % normal vectors
    nx = normal(1);
    ny = normal(2);
    
    % Left state
    rL = qL(1);
    uL = qL(2)/rL;
    vL = qL(3)/rL;
    vnL = uL*nx+vL*ny;
    pL = (gamma-1)*( qL(4) - rL*(uL^2+vL^2)/2 );
    aL = sqrt(gamma*pL/rL);
    HL = ( qL(4) + pL ) / rL;
    
    % Right state
    rR = qR(1);
    uR = qR(2)/rR;
    vR = qR(3)/rR;
    vnR = uR*nx+vR*ny;
    pR = (gamma-1)*( qR(4) - rR*(uR^2+vR^2)/2 );
    aR = sqrt(gamma*pR/rR);
    HR = ( qR(4) + pR ) / rR;
       
    % Left and Right fluxes
    FL=[rL*vnL; rL*vnL*uL + pL*nx; rL*vnL*vL + pL*ny; rL*vnL*HL];
    FR=[rR*vnR; rR*vnR*uR + pR*nx; rR*vnR*vR + pR*ny; rR*vnR*HR];

    % Compute guess pressure from PVRS Riemann solver
    PPV  = max(0 , 0.5*(pL+pR) + 0.5*(vnL-vnR) * (0.25*(rL+rR)*(aL+aR)));
    pmin = min(pL,pR);
    pmax = max(pL,pR);
    Qmax = pmax/pmin;
    Quser= 2.0; % <--- parameter manually set (I don't like this!)
    
     if (Qmax <= Quser) && (pmin <= PPV) && (PPV <= pmax)
     % Select PRVS Riemann solver
         pM = PPV;
      else
         if PPV < pmin
         % Select Two-Rarefaction Riemann solver
            PQ  = (pL/pR)^(gamma - 1.0)/(2.0*gamma);
            uM  = (PQ*vnL/aL + vnR/aR + 2/(gamma-1)*(PQ-1.0))/(PQ/aL+1.0/aR);
            PTL = 1 + (gamma-1)/2.0*(vnL - uM)/aL;
            PTR = 1 + (gamma-1)/2.0*(uM - vnR)/aR;
            pM  = 0.5*(pL*PTL^(2*gamma/(gamma-1)) + pR*PTR^(2*gamma/(gamma-1)));
         else 
         % Use Two-Shock Riemann solver with PVRS as estimate
            GEL = sqrt((2/(gamma+1)/rL)/((gamma-1)/(gamma+1)*pL + PPV));
            GER = sqrt((2/(gamma+1)/rR)/((gamma-1)/(gamma+1)*pR + PPV));
            pM  = (GEL*pL + GER*pR - (vnR - vnL))/(GEL + GER);
         end
      end

    % Estimate wave speeds: SL, SR and SM (Toro, 1994)
    if pM>pL; zL=sqrt(1+(gamma+1)/(2*gamma)*(pM/pL-1)); else, zL=1; end    
    if pM>pR; zR=sqrt(1+(gamma+1)/(2*gamma)*(pM/pR-1)); else, zR=1; end
  
	SL = vnL - aL*zL;
    SR = vnR + aR*zR;
    SM = (pL-pR + rR*vnR*(SR-vnR) - rL*vnL*(SL-vnL))/(rR*(SR-vnR) - rL*(SL-vnL));
    
    % Compute the HLL flux.
    if (0 <= SL)  % Right-going supersonic flow
        HLLC = FL;
    elseif (SL <= 0) && (0 <= SM)	% Subsonic flow to the right
        qsL = rL*(SL-vnL)/(SL-SM)*[1; SM*nx+uL*abs(ny); SM*ny+vL*abs(nx); qL(4)/rL + (SM-vnL)*(SM+pL/(rL*(SL-vnL)))];
        HLLC = FL + SL*(qsL - qL);
    elseif (SM <= 0) && (0 <= SR)	% Subsonic flow to the Left
        qsR = rR*(SR-vnR)/(SR-SM)*[1; SM*nx+uR*abs(ny); SM*ny+vR*abs(nx); qR(4)/rR + (SM-vnR)*(SM+pR/(rR*(SR-vnR)))];
        HLLC = FR + SR*(qsR - qR);
    elseif	(0 >= SR) % Left-going supersonic flow
        HLLC = FR;
    end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% WENO and Polynomail reconstructions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [qn,qp] = WENO5recon_X(w,N)
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
I=3:(N-3); % R:3 stencil size

%% Left State Extrapolation $u_{i+1/2}^{-}$
vmm = reshape(w(:,I-2,:),1,[]);
vm  = reshape(w(:,I-1,:),1,[]);
v   = reshape(w(:, I ,:),1,[]);
vp  = reshape(w(:,I+1,:),1,[]);
vpp = reshape(w(:,I+2,:),1,[]);

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
qn  = w0n.*(2*vmm - 7*vm + 11*v)/6 ...
    + w1n.*( -vm  + 5*v  + 2*vp)/6 ...
    + w2n.*(2*v   + 5*vp - vpp )/6;

%% Right State Extrapolation $u_{i+1/2}^{+}$ 
umm = reshape(w(:,I-1,:),1,[]);
um  = reshape(w(:, I ,:),1,[]);
u   = reshape(w(:,I+1,:),1,[]);
up  = reshape(w(:,I+2,:),1,[]);
upp = reshape(w(:,I+3,:),1,[]);

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
qp  = w0p.*( -umm + 5*um + 2*u  )/6 ...
	+ w1p.*( 2*um + 5*u  - up   )/6 ...
	+ w2p.*(11*u  - 7*up + 2*upp)/6;
end

function [qn,qp] = WENO5recon_Y(w,N)
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
vmm = reshape(w(I-2,:,:),1,[]);
vm  = reshape(w(I-1,:,:),1,[]);
v   = reshape(w( I ,:,:),1,[]);
vp  = reshape(w(I+1,:,:),1,[]);
vpp = reshape(w(I+2,:,:),1,[]);

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
qn  = w0n.*(2*vmm - 7*vm + 11*v)/6 ...
    + w1n.*( -vm  + 5*v  + 2*vp)/6 ...
    + w2n.*(2*v   + 5*vp - vpp )/6;

%% Right State Extrapolation $u_{i+1/2}^{+}$ 
umm = reshape(w(I-1,:,:),1,[]);
um  = reshape(w( I ,:,:),1,[]);
u   = reshape(w(I+1,:,:),1,[]);
up  = reshape(w(I+2,:,:),1,[]);
upp = reshape(w(I+3,:,:),1,[]);

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
qp  = w0p.*( -umm + 5*um + 2*u  )/6 ...
	+ w1p.*( 2*um + 5*u  - up   )/6 ...
	+ w2p.*(11*u  - 7*up + 2*upp)/6;
end

function [qn,qp] = POLY5recon_X(w,N)
% Direct Polynomial reconstruction
I=3:(N-3); % R:3 stencil size

%% Left Flux: f_{i+1/2}^{-}
vmm = reshape(w(:,I-2,:),1,[]);
vm  = reshape(w(:,I-1,:),1,[]);
v   = reshape(w(:, I ,:),1,[]);
vp  = reshape(w(:,I+1,:),1,[]);
vpp = reshape(w(:,I+2,:),1,[]);

% Numerical Flux at cell boundary, $u_{i+1/2}^{-}$;
qn = ( 2*vmm - 13*vm + 47*v + 27*vp - 3*vpp)/60;

%% Right Flux: f_{i+1/2}^{+}
umm = reshape(w(:,I-1,:),1,[]);
um  = reshape(w(:, I ,:),1,[]);
u   = reshape(w(:,I+1,:),1,[]);
up  = reshape(w(:,I+2,:),1,[]);
upp = reshape(w(:,I+3,:),1,[]);

% Numerical Flux at cell boundary, $u_{i+1/2}^{+}$;
qp = (-3*umm + 27*um + 47*u - 13*up + 2*upp)/60;
end

function [qn,qp] = POLY5recon_Y(w,N)
% The stencil size
I=3:(N-3); 

%% Left Flux: f_{i+1/2}^{-}
vmm = reshape(w(I-2,:,:),1,[]);
vm  = reshape(w(I-1,:,:),1,[]);
v   = reshape(w( I ,:,:),1,[]);
vp  = reshape(w(I+1,:,:),1,[]);
vpp = reshape(w(I+2,:,:),1,[]);

% Numerical Flux at cell boundary, $u_{i+1/2}^{-}$;
qn = ( 2*vmm - 13*vm + 47*v + 27*vp - 3*vpp)/60;

%% Right Flux: f_{i+1/2}^{+}
umm = reshape(w(I-1,:,:),1,[]);
um  = reshape(w( I ,:,:),1,[]);
u   = reshape(w(I+1,:,:),1,[]);
up  = reshape(w(I+2,:,:),1,[]);
upp = reshape(w(I+3,:,:),1,[]);

% Numerical Flux at cell boundary, $u_{i+1/2}^{+}$;
qp = (-3*umm + 27*um + 47*u - 13*up + 2*upp)/60;
end

function [qn,qp] = WENO7recon_X(w,N)
% *************************************************************************
% Input: u(i) = [u(i-2) u(i-1) u(i) u(i+1) u(i+2)];
% Output: res = df/dx;
%
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
vmmm= reshape(w(:,I-3,:),1,[]);
vmm = reshape(w(:,I-2,:),1,[]);
vm  = reshape(w(:,I-1,:),1,[]);
vo  = reshape(w(:, I ,:),1,[]);
vp  = reshape(w(:,I+1,:),1,[]);
vpp = reshape(w(:,I+2,:),1,[]);
vppp= reshape(w(:,I+3,:),1,[]);

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
qn = w0n.*(-3*vmmm + 13*vmm - 23*vm  + 25*vo  )/12 + ...
     w1n.*( 1*vmm  - 5*vm   + 13*vo  +  3*vp  )/12 + ...
     w2n.*(-1*vm   + 7*vo   +  7*vp  -  1*vpp )/12 + ...
     w3n.*( 3*vo   + 13*vp  -  5*vpp +  1*vppp)/12;

%% Left Flux: f_{i+1/2}^{+}
ummm= reshape(w(:,I-2,:),1,[]);
umm = reshape(w(:,I-1,:),1,[]);
um  = reshape(w(:, I ,:),1,[]);
uo  = reshape(w(:,I+1,:),1,[]);
up  = reshape(w(:,I+2,:),1,[]);
upp = reshape(w(:,I+3,:),1,[]);
uppp= reshape(w(:,I+4,:),1,[]);

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
qp = w0p.*( 1*ummm - 5*umm  + 13*um  +  3*uo  )/12 + ...
     w1p.*(-1*umm  + 7*um   +  7*uo  -  1*up  )/12 + ... 
     w2p.*( 3*um   + 13*uo  -  5*up  +  1*upp )/12 + ...
     w3p.*(25*uo   - 23*up  + 13*upp -  3*uppp)/12;
end


function [qn,qp] = WENO7recon_Y(w,N)
% *************************************************************************
% Input: u(i) = [u(i-2) u(i-1) u(i) u(i+1) u(i+2)];
% Output: res = df/dx;
%
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
vmmm= reshape(w(I-3,:,:),1,[]);
vmm = reshape(w(I-2,:,:),1,[]);
vm  = reshape(w(I-1,:,:),1,[]);
vo  = reshape(w( I ,:,:),1,[]);
vp  = reshape(w(I+1,:,:),1,[]);
vpp = reshape(w(I+2,:,:),1,[]);
vppp= reshape(w(I+3,:,:),1,[]);

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
qn = w0n.*(-3*vmmm + 13*vmm - 23*vm  + 25*vo  )/12 + ...
     w1n.*( 1*vmm  - 5*vm   + 13*vo  +  3*vp  )/12 + ...
     w2n.*(-1*vm   + 7*vo   +  7*vp  -  1*vpp )/12 + ...
     w3n.*( 3*vo   + 13*vp  -  5*vpp +  1*vppp)/12;

%% Left Flux: f_{i+1/2}^{+}
ummm= reshape(w(I-2,:,:),1,[]);
umm = reshape(w(I-1,:,:),1,[]);
um  = reshape(w( I ,:,:),1,[]);
uo  = reshape(w(I+1,:,:),1,[]);
up  = reshape(w(I+2,:,:),1,[]);
upp = reshape(w(I+3,:,:),1,[]);
uppp= reshape(w(I+4,:,:),1,[]);

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
qp = w0p.*( 1*ummm - 5*umm  + 13*um  +  3*uo  )/12 + ...
     w1p.*(-1*umm  + 7*um   +  7*uo  -  1*up  )/12 + ... 
     w2p.*( 3*um   + 13*uo  -  5*up  +  1*upp )/12 + ...
     w3p.*(25*uo   - 23*up  + 13*upp -  3*uppp)/12;
end

function [qn,qp] = POLY7recon_X(w,N)
% The stencil size
I=4:(N-4);  

%% Left Flux: f_{i+1/2}^{-}
vmmm= reshape(w(:,I-3,:),1,[]);
vmm = reshape(w(:,I-2,:),1,[]);
vm  = reshape(w(:,I-1,:),1,[]);
vo  = reshape(w(:, I ,:),1,[]);
vp  = reshape(w(:,I+1,:),1,[]);
vpp = reshape(w(:,I+2,:),1,[]);
vppp= reshape(w(:,I+3,:),1,[]);

% Numerical Flux at cell boundary, $u_{i+1/2}^{-}$;
qn = (-3*vmmm + 25*vmm - 101*vm  + 319*vo + 214*vp - 38*vpp + 4*vppp)/420;

%% Right Flux: f_{i+1/2}^{+}
ummm= reshape(w(:,I-2,:),1,[]);
umm = reshape(w(:,I-1,:),1,[]);
um  = reshape(w(:, I ,:),1,[]);
uo  = reshape(w(:,I+1,:),1,[]);
up  = reshape(w(:,I+2,:),1,[]);
upp = reshape(w(:,I+3,:),1,[]);
uppp= reshape(w(:,I+4,:),1,[]);

% Numerical Flux at cell boundary, $u_{i+1/2}^{+}$;
qp = (4*ummm - 38*umm  + 214*um  + 319*uo - 101*up + 25*upp - 3*uppp)/420;
end

function [qn,qp] = POLY7recon_Y(w,N)
% The stencil size
I=4:(N-4);  

%% Left Flux: f_{i+1/2}^{-}
vmmm= reshape(w(I-3,:,:),1,[]);
vmm = reshape(w(I-2,:,:),1,[]);
vm  = reshape(w(I-1,:,:),1,[]);
vo  = reshape(w( I ,:,:),1,[]);
vp  = reshape(w(I+1,:,:),1,[]);
vpp = reshape(w(I+2,:,:),1,[]);
vppp= reshape(w(I+3,:,:),1,[]);

% Numerical Flux at cell boundary, $u_{i+1/2}^{-}$;
qn = (-3*vmmm + 25*vmm - 101*vm  + 319*vo + 214*vp - 38*vpp + 4*vppp)/420;

%% Right Flux: f_{i+1/2}^{+}
ummm= reshape(w(I-2,:,:),1,[]);
umm = reshape(w(I-1,:,:),1,[]);
um  = reshape(w( I ,:,:),1,[]);
uo  = reshape(w(I+1,:,:),1,[]);
up  = reshape(w(I+2,:,:),1,[]);
upp = reshape(w(I+3,:,:),1,[]);
uppp= reshape(w(I+4,:,:),1,[]);

% Numerical Flux at cell boundary, $u_{i+1/2}^{+}$;
qp = (4*ummm - 38*umm  + 214*um  + 319*uo - 101*up + 25*upp - 3*uppp)/420;
end