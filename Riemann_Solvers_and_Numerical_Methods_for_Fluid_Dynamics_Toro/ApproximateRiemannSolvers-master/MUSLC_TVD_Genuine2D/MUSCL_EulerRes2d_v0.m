function [res] = MUSCL_EulerRes2d_v0(q,~,dx,dy,N,M,limiter,fluxMethod)
%   A genuine 2d HLLE Riemnan solver for Euler Equations using a Monotonic
%   Upstreat Centered Scheme for Conservation Laws (MUSCL).
%  
%   e.g. where: limiter='MC'; fluxMethod='HLLE1d';
%
%   Flux at j+1/2
% 
%     j+1/2         Cell's grid:
%   | wL|   |
%   |  /|wR |           1   2   3   4        N-2 N-1  N
%   | / |\  |   {x=0} |-o-|-o-|-o-|-o-| ... |-o-|-o-|-o-| {x=L}
%   |/  | \ |             1   2   3   4        N-2 N-1  
%   |   |  \|
%   |   |   |       NC: Here cells 1 and N are ghost cells
%     j  j+1            faces 1 and N-1, are the real boundary faces.
%
%   q = cat(3, r, r.*u, r.*v, r.*E);
%   F = cat(3, r.*u, r.*u.^2+p, r.*u.*v, u.*(r.*E+p));
%   G = cat(3, r.*v, r.*u.*v, r.*v.^2+p, v.*(r.*E+p));
%
% Written by Manuel Diaz, NTU, 05.25.2015.
    res = zeros(M,N,4);

    % Normal unitary face vectors: (nx,ny)
    % normals = {[0,1], [1,0], [0,-1], [-1,0]}; % i.e.: [N, E, S, W] 

    % Build cells
    cell(M,N).all = M*N;
    for i = 1:M
        for j = 1:N
            cell(i,j).q = [q(i,j,1);q(i,j,2);q(i,j,3);q(i,j,4)];
            cell(i,j).qN = zeros(4,1);
            cell(i,j).qS = zeros(4,1);
            cell(i,j).qE = zeros(4,1);
            cell(i,j).qW = zeros(4,1);
            cell(i,j).res = zeros(4,1);
        end
    end

    % Compute and limit slopes at cells (i,j)
    for i = 2:M-1       % only internal cells
        for j = 2:N-1   % only internal cells
            for k = 1:4
                dqw = (cell( i,j ).q(k) - cell(i,j-1).q(k)); % du btn j and j-1
                dqe = (cell(i,j+1).q(k) - cell( i,j ).q(k)); % du btn j+1 and j
                dqs = (cell( i,j ).q(k) - cell(i-1,j).q(k)); % du btn i and i-1
                dqn = (cell(i+1,j).q(k) - cell( i,j ).q(k)); % du btn i+1 and i
                switch limiter
                    case 'MC' % MC limiter
                        % Find dq_j = minmod{fwd diff, bwd diff, cntrl diff}
                        dqc = (cell(i,j+1).q(k)-cell(i,j-1).q(k))/2; % du btn j+1 and j-1
                        dqdx = minmod([2*dqw,2*dqe,dqc]);
                        dqc = (cell(i+1,j).q(k)-cell(i-1,j).q(k))/2; % du btn j+1 and j-1
                        dqdy = minmod([2*dqs,2*dqn,dqc]);
                    case 'MM' % Minmod limiter
                        % Find dq_j = minmod{fwd diff, bwd diff}
                        dqdx = minmod([dqw,dqe]);
                        dqdy = minmod([dqs,dqn]);
                    case 'VA' % Van Albada limiter
                        % Find dq_j = minmod{fwd diff, bwd diff}
                        dqdx = vanalbada(dqw,dqe,dx);
                        dqdy = vanalbada(dqs,dqn,dy);
                    case 'VL' % Van Leer limiter
                        % Find dq_j = vanAlvada{fwd diff, bwd diff, h }
                        dqdx = vanLeer(dqw,dqe);
                        dqdy = vanLeer(dqs,dqn);
                end
                cell(i,j).qE(k) = cell(i,j).q(k) + dqdx/2; % q_{i,j+1/2}^{-} from (i,j)
                cell(i,j).qW(k) = cell(i,j).q(k) - dqdx/2; % q_{i,j-1/2}^{+} from (i,j)
                cell(i,j).qN(k) = cell(i,j).q(k) + dqdy/2; % q_{i+1/2,j}^{-} from (i,j)
                cell(i,j).qS(k) = cell(i,j).q(k) - dqdy/2; % q_{i-1/2,j}^{+} from (i,j)
            end
        end
    end
    
	%%%%%%%%%%%%%
    % Residuals %
    %%%%%%%%%%%%%
    
    % Compute residuals x-direction
    for i = 2:M-1     % internal cells
        for j = 2:N-2 % internal faces
            % Left (inside) and Right (outside) extrapolated q-values at the boundaries
            qxL = [cell( i,j ).qE]; % q_{i,j+1/2}^{-}
            qxR = [cell(i,j+1).qW]; % q_{i,j+1/2}^{+}
            % compute flux at j+1/2 using
            switch fluxMethod
                case 'HLLE1d', flux = HLLE1Dflux(qxL,qxR,[1,0]); % F_{i,j+1/2}
                otherwise, error('flux option not available');
            end
            % contributions to the residual of cell (i,j) and cells around it
            cell( i,j ).res = cell( i,j ).res + flux/dx;
            cell(i,j+1).res = cell(i,j+1).res - flux/dx;
        end
    end
    
    % Compute residuals y-direction
    for i = 2:M-2     % internal faces
        for j = 2:N-1 % internal cells
            % Left (inside) and Right (outside) extrapolated q-values at the boundaries
            qyL = [cell( i,j ).qN]; % q_{i+1/2,j}^{-}
            qyR = [cell(i+1,j).qS]; % q_{i+1/2,j}^{+}
            % compute flux at j+1/2 using
            switch fluxMethod
                case 'HLLE1d', flux = HLLE1Dflux(qyL,qyR,[0,1]); % F_{i+1/2,j}
            end
            % contributions to the residual of cell (i,j) and cells around it
            cell( i,j ).res = cell( i,j ).res + flux/dy;
            cell(i+1,j).res = cell(i+1,j).res - flux/dy;
        end
    end
    
    %%%%%%%%%%%
    % set BCs %
    %%%%%%%%%%%
    
    % Flux contribution of the MOST NORTH FACE: north face of cells j=M-1.
    for j = 2:N-1
        qR = cell(M-1,j).qS;     qL = qR;
        switch fluxMethod
            case 'HLLE1d', flux = HLLE1Dflux(qL,qR,[0,1]); % F_{i+1/2,j}
        end
        cell(M-1,j).res = cell(M-1,j).res + flux/dy;
    end
    
    % Flux contribution of the MOST EAST FACE: east face of cell j=N-1.
    for i = 2:M-1
        qR = cell(i,N-1).qW;     qL = qR;
        switch fluxMethod
            case 'HLLE1d', flux = HLLE1Dflux(qL,qR,[1,0]); % F_{i,j+1/2}
        end
        cell(i,N-1).res = cell(i,N-1).res + flux/dx;
    end
    
    % Flux contribution of the MOST SOUTH FACE: south face of cells j=2.
    for j = 2:N-1
        qR = cell(2,j).qN;     qL = qR;
        switch fluxMethod
            case 'HLLE1d', flux = HLLE1Dflux(qL,qR,[0,-1]); % F_{i-1/2,j}
        end
        cell(2,j).res = cell(2,j).res + flux/dy;
    end
    
    % Flux contribution of the MOST WEST FACE: west face of cells j=2.
    for i = 2:M-1
        qR = cell(i,2).qE;     qL = qR;
        switch fluxMethod
            case 'HLLE1d', flux = HLLE1Dflux(qL,qR,[-1,0]); % F_{i,j-1/2}
        end
        cell(i,2).res = cell(i,2).res + flux/dx;
    end
    
    % Prepare residual as layers: [rho, rho*u, rho*v, rho*E]
    parfor i = 2:M-1
        for j = 2:N-1
            res(i,j,:) = cell(i,j).res;
        end
    end
    
    % Debug
    % Q=[cell(:,:).res]; Q=reshape(Q(1,:),M,N); surf(Q);
end % 

%%%%%%%%%%%%%%%%%%%%%%%
% Auxiliary Functions %
%%%%%%%%%%%%%%%%%%%%%%%

function mm = minmod(v)
    % Using Harten's generalized definition
    % minmod: zero if opposite sign, otherwise the one of smaller magnitude.
    s = sum(sign(v))/numel(v); 
    if abs(s)==1; mm = s*min(abs(v(:))); else, mm=0; end
    %m=size(v,1); mm=zeros(size(v,1),1); s=sum(sign(v),2)/m; ids=find(abs(s)==1);
    %if(~isempty(ids)); mm(ids)=s(ids).*min(abs(v(ids,:)),[],2); end
end

function va = vanalbada(da,db,h)
    % Van Albada Slope Limiter Function
    % vanAlbada: extend the simetric formulation of the van leer limiter
    eps2=(0.3*h)^3; 
    va=0.5*(sign(da)*sign(db)+1)*((db^2+eps2)*da+(da^2+eps2)*db)/(da^2+db^2+2*eps2);
end

function vl = vanLeer(da,db)
    % Van Leer Slope Limiter Function
    vl = 0; if bd~=0, r=da/db; vl=(r+abs(r))/(1+abs(r)); end
end

function HLLE = HLLE1Dflux(qL,qR,normal)
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
