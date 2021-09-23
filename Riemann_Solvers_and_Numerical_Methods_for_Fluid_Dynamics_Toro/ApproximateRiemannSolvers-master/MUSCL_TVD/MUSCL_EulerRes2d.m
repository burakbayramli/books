function [res] = MUSCL_EulerRes2d(q,smax,gamma,dx,dy,N,M,limiter,fluxMethod)
%   MUSCL Monotonic Upstreat Centered Scheme for Conservation Laws
%   Van Leer's MUSCL reconstruction scheme using piece wise linear
%   reconstruction
%  
%   e.g. where: limiter='MC'; fluxMethod='AUSM';
%
%   Flux at j+1/2
% 
%     j+1/2         Cell's grid:
%   | wL|   |
%   |  /|wR |           1   2   3   4        N-2 N-1  N
%   | / |\  |   {x=0} |-o-|-o-|-o-|-o-| ... |-o-|-o-|-o-| {x=L}
%   |/  | \ |         1   2   3   4   5        N-1  N  N+1
%   |   |  \|
%   |   |   |       NC: Here cells 1 and N are ghost cells
%     j  j+1            faces 2 and N, are the real boundary faces.
%
%   q = cat(3, r, r.*u, r.*v, r.*E);
%   F = cat(3, r.*u, r.*u.^2+p, r.*u.*v, u.*(r.*E+p));
%   G = cat(3, r.*v, r.*u.*v, r.*v.^2+p, v.*(r.*E+p));
%
% Written by Manuel Diaz, NTU, 04.29.2015.
    res = zeros(M,N,4);

    % Normal unitary face vectors: (nx,ny)
    %normals = {[0,1], [1,0], [0,-1], [-1,0]}; % i.e.: [N, E, S, W] 

    % Build cells
    cell(M,N).all = M*N;
    for i = 1:M
        for j = 1:N
            cell(i,j).q = [q(i,j,1);q(i,j,2);q(i,j,3);q(i,j,4)];
            cell(i,j).dqdy = zeros(4,1);
            cell(i,j).dqdx = zeros(4,1);
            cell(i,j).res = zeros(4,1);
        end
    end

    % Compute and limit slopes at cells (i,j)
    for i = 2:M-1       % only internal cells
        for j = 2:N-1   % only internal cells
            for k = 1:4
                switch limiter
                    case 'MC' % MC limiter
                        % Find dq_j = minmod{fwd diff, bwd diff, cntrl diff}
                        dqw = 2*(cell(i, j ).q(k) - cell(i,j-1).q(k))/dx; % du btn j and j-1
                        dqe = 2*(cell(i,j+1).q(k) - cell(i, j ).q(k))/dx; % du btn j+1 and j
                        dqc = (cell(i,j+1).q(k)-cell(i,j-1).q(k))/(2*dx); % du btn j+1 and j-1
                        cell(i,j).dqdx(k) = minmod([dqw,dqe,dqc]);
                        dqs = 2*(cell( i ,j).q(k) - cell(i-1,j).q(k))/dy; % du btn i and i-1
                        dqn = 2*(cell(i+1,j).q(k) - cell( i ,j).q(k))/dy; % du btn i+1 and i
                        dqc = (cell(i+1,j).q(k)-cell(i-1,j).q(k))/(2*dy); % du btn j+1 and j-1
                        cell(i,j).dqdy(k) = minmod([dqs,dqn,dqc]);
                    case 'MM' % Minmod limiter
                        % Find dq_j = minmod{fwd diff, bwd diff}
                        dqw = (cell(i, j ).q(k) - cell(i,j-1).q(k))/dx; % du btn j and j-1
                        dqe = (cell(i,j+1).q(k) - cell(i, j ).q(k))/dx; % du btn j+1 and j
                        cell(i,j).dqdx(k) = minmod([dqw,dqe]);
                        dqs = (cell( i ,j).q(k) - cell(i-1,j).q(k))/dy; % du btn i and i-1
                        dqn = (cell(i+1,j).q(k) - cell( i ,j).q(k))/dy; % du btn i+1 and i
                        cell(i,j).dqdy(k) = minmod([dqs,dqn]);
                    case 'VA' % Van Albada limiter
                        % Find dq_j = minmod{fwd diff, bwd diff}
                        dqw = (cell(i, j ).q(k) - cell(i,j-1).q(k))/dx; % du btn j and j-1
                        dqe = (cell(i,j+1).q(k) - cell(i, j ).q(k))/dx; % du btn j+1 and j
                        cell(i,j).dqdx(k) = vanalbada(dqw,dqe,dx);
                        dqs = (cell( i ,j).q(k) - cell(i-1,j).q(k))/dy; % du btn i and i-1
                        dqn = (cell(i+1,j).q(k) - cell( i ,j).q(k))/dy; % du btn i+1 and i
                        cell(i,j).dqdy(k) = vanalbada(dqs,dqn,dy);
                end
            end
        end
    end

	%%%%%%%%%%%%%
    % Residuals %
    %%%%%%%%%%%%%
    
    % Compute residuals x-direction
    for i = 2:M-1
        for j = 2:N-2
            % Left (inside) and Right (outside) extrapolated q-values at the boundaries
            qxL = cell( i,j ).q + cell( i,j ).dqdx*dx/2; % q_{i,j+1/2}^{-} from (i,j)
            qxR = cell(i,j+1).q - cell(i,j+1).dqdx*dx/2; % q_{i,j+1/2}^{+} from (i,j+1)
            % compute flux at j+1/2 using
            switch fluxMethod
                case 'LF'  % Lax-Friedrichs
                    flux = LFflux(qxL,qxR,gamma,[1,0],smax); % F_{i,j+1/2}
                case 'RUS' % Rusanov or local Lax-Friedrichs
                    flux = RUSflux(qxL,qxR,gamma,[1,0]);    % F_{i,j+1/2}
                case 'ROE' % Roe flux
                    flux = ROEflux(qxL,qxR,gamma,[1,0]);    % F_{i,j+1/2}
                case 'HLLE' % HLLE flux
                    flux = HLLEflux(qxL,qxR,gamma,[1,0]);   % F_{i,j+1/2}
               	case 'HLLC' % HLLC flux
                    flux = HLLCflux(qxL,qxR,gamma,[1,0]);	% F_{i,j+1/2}
                otherwise
                    error('flux option not available')
            end
            % contributions to the residual of cell (i,j) and cells around it
            cell( i,j ).res = cell( i,j ).res + flux/dx;
            cell(i,j+1).res = cell(i,j+1).res - flux/dx;
        end
    end
    
    % Compute residuals y-direction
    for i = 2:M-2
        for j = 2:N-1
            % Left (inside) and Right (outside) extrapolated q-values at the boundaries
            qyL = cell( i,j ).q + cell( i,j ).dqdy*dy/2; % q_{i+1/2,j}^{-} from (i,j)
            qyR = cell(i+1,j).q - cell(i+1,j).dqdy*dy/2; % q_{i+1/2,j}^{+} from (i,j+1)
            % compute flux at j+1/2 using
            switch fluxMethod
                case 'LF'  % Lax-Friedrichs
                    flux = LFflux(qyL,qyR,gamma,[0,1],smax); % F_{i+1/2,j}
                case 'RUS' % Rusanov or local Lax-Friedrichs
                    flux = RUSflux(qyL,qyR,gamma,[0,1]);	% F_{i+1/2,j}
                case 'ROE' % Roe flux
                    flux = ROEflux(qyL,qyR,gamma,[0,1]);	% F_{i+1/2,j}
                case 'HLLE' % HLLE flux
                    flux = HLLEflux(qyL,qyR,gamma,[0,1]);	% F_{i+1/2,j}
                case 'HLLC' % HLLC flux
                    flux = HLLCflux(qyL,qyR,gamma,[0,1]);	% F_{i+1/2,j}
                otherwise
                    error('flux option not available')
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
        qR = cell(M-1,j).q + cell(M-1,j).dqdy*dy/2;     qL = qR;
        switch fluxMethod
            case 'LF'  % Lax-Friedrichs
                flux = LFflux(qL,qR,gamma,[0,1],smax);	% F_{i+1/2,j}
            case 'RUS' % Rusanov or local Lax-Friedrichs
                flux = RUSflux(qL,qR,gamma,[0,1]);      % F_{i+1/2,j}
            case 'ROE' % Roe flux
                flux = ROEflux(qL,qR,gamma,[0,1]);      % F_{i+1/2,j}
            case 'HLLE' % HLLE flux
                flux = HLLEflux(qL,qR,gamma,[0,1]);     % F_{i+1/2,j}
          	case 'HLLC' % HLLC flux
                flux = HLLCflux(qL,qR,gamma,[0,1]);     % F_{i+1/2,j}
        end
        cell(M-1,j).res = cell(M-1,j).res + flux/dy;
    end
    
    % Flux contribution of the MOST EAST FACE: east face of cell j=N-1.
    for i = 2:M-1
        qR = cell(i,N-1).q + cell(i,N-1).dqdx*dx/2;     qL = qR;
        switch fluxMethod
            case 'LF'  % Lax-Friedrichs
                flux = LFflux(qL,qR,gamma,[1,0],smax);	% F_{j,i+1/2}
            case 'RUS' % Rusanov or local Lax-Friedrichs
                flux = RUSflux(qL,qR,gamma,[1,0]);      % F_{i,j+1/2}
            case 'ROE' % Roe flux
                flux = ROEflux(qL,qR,gamma,[1,0]);      % F_{i,j+1/2}
            case 'HLLE' % HLLE flux
                flux = HLLEflux(qL,qR,gamma,[1,0]);     % F_{i,j+1/2}
          	case 'HLLC' % HLLC flux
                flux = HLLCflux(qL,qR,gamma,[1,0]);     % F_{i,j+1/2}
        end
        cell(i,N-1).res = cell(i,N-1).res + flux/dx;
    end
    
    % Flux contribution of the MOST SOUTH FACE: south face of cells j=2.
    for j = 2:N-1
        qR = cell(2,j).q - cell(2,j).dqdy*dy/2;     qL = qR;
        switch fluxMethod
            case 'LF'  % Lax-Friedrichs
                flux = LFflux(qL,qR,gamma,[0,-1],smax); % F_{i-1/2,j}
            case 'RUS' % Rusanov or local Lax-Friedrichs
                flux = RUSflux(qL,qR,gamma,[0,-1]);     % F_{i-1/2,j}
            case 'ROE' % Roe flux
                flux = ROEflux(qL,qR,gamma,[0,-1]);     % F_{i-1/2,j}
            case 'HLLE' % HLLE flux
                flux = HLLEflux(qL,qR,gamma,[0,-1]);    % F_{i-1/2,j}
           	case 'HLLC' % HLLC flux
                flux = HLLCflux(qL,qR,gamma,[0,-1]);    % F_{i-1/2,j}
        end
        cell(2,j).res = cell(2,j).res + flux/dy;
    end
    
    % Flux contribution of the MOST WEST FACE: west face of cells j=2.
    for i = 2:M-1
        qR = cell(i,2).q - cell(i,2).dqdx*dx/2;     qL = qR;
        switch fluxMethod
            case 'LF'  % Lax-Friedrichs
                flux = LFflux(qL,qR,gamma,[-1,0],smax); % F_{i,j-1/2}
            case 'RUS' % Rusanov or local Lax-Friedrichs
                flux = RUSflux(qL,qR,gamma,[-1,0]);     % F_{i,j-1/2}
            case 'ROE' % Roe flux
                flux = ROEflux(qL,qR,gamma,[-1,0]);     % F_{i,j-1/2}
            case 'HLLE' % HLLE flux
                flux = HLLEflux(qL,qR,gamma,[-1,0]);	% F_{i,j-1/2}
          	case 'HLLC' % HLLC flux
                flux = HLLCflux(qL,qR,gamma,[-1,0]);	% F_{i,j-1/2}
        end
        cell(i,2).res = cell(i,2).res + flux/dx;
    end
    
    % Prepare residual as layers: [rho, rho*u, rho*v, rho*E]
    parfor i = 1:M
        for j = 1:N
            res(i,j,:) = cell(i,j).res;
        end
    end
end

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

function LF = LFflux(qL,qR,gamma,normal,smax)
    % Lax-Friedrichs flux

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

function Rusanov = RUSflux(qL,qR,gamma,normal)
    % Rusanov flux 
    
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

function Roe = ROEflux(qL,qR,gamma,normal)
    % Compute Roe flux
    
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

function HLLE = HLLEflux(qL,qR,gamma,normal)
    % Compute HLLE flux

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

function HLLC = HLLCflux(qL,qR,gamma,normal)
    % Compute HLLC flux

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
