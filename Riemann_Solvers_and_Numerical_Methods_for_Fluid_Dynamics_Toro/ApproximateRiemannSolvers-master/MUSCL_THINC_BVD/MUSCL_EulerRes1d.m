function [res] = MUSCL_EulerRes1d(smax,q,N,dx,limiter,fluxMethod)
%   MUSCL Monotonic Upstreat Centered Scheme for Conservation Laws
%   Van Leer's MUSCL reconstruction scheme using piece wise linear
%   reconstruction
%  
%   where: limiter='MC'; fluxMethod='AUSM';
%
%   Flux at j+1/2
% 
%     j+1/2         Cell's grid:
%   | wL|   |
%   |  /|wR |           1   2   3   4        N-2 N-1  N
%   | / |\  |   {x=0} |-o-|-o-|-o-|-o-| ... |-o-|-o-|-o-| {x=L}
%   |/  | \ |         1   2   3   4   5        N-1  N  N+1
%   |   |  \|
%   |   |   |       NC: Here cells 1 and N are ghost cells%
%     j  j+1
%
% Written by Manuel Diaz, NTU, 04.29.2015.

    global gamma

    % Compute and limit slopes
    res=zeros(3,N); dq=zeros(3,N); 
    flux=zeros(3,N-1); qL=zeros(3,N-1); qR=zeros(3,N-1);
    for i = 1:3
        for j = 2:N-1 % for all internal faces
            switch limiter
                case 'MC' % MC limiter
                    % Find dq_j = minmod{fwd diff, bwd diff, cntrl diff}
                    dqR = 2*(q(i,j+1) - q(i,j));
                    dqL = 2*(q(i,j) - q(i,j-1));
                    dqC = (q(i,j+1) - q(i,j-1))/2;
                    dq(i,j) = minmod([dqR,dqL,dqC]);
                case 'MM' % Minmod limiter
                    % Find dq_j = minmod{fwd diff, bwd diff}
                    dqR = (q(i,j+1) - q(i,j));
                    dqL = (q(i,j) - q(i,j-1));
                    dq(i,j) = minmod([dqR,dqL]);
                case 'VA' % Van Albada limiter
                    dqR = (q(i,j+1) - q(i,j));
                    dqL = (q(i,j) - q(i,j-1));
                    dq(i,j) = vanalbada(dqR,dqL,dx);
            end
        end
    end

    % Left and Right extrapolated q-values at the boundary j+1/2
    for j = 2:N-2 % for the domain cells
        qL(:,j) = q(:, j ) + dq(:, j )/2;	% q_{j+1/2}^{-} from j
        qR(:,j) = q(:,j+1) - dq(:,j+1)/2;	% q_{j+1/2}^{+} from j+1
    end

    % Flux contribution to the residual of every cell
    for j = 2:N-2 % for all faces the domain cells
        % compute flux at j+1/2
        switch fluxMethod
            case 'LF' % Lax-Friedrichs
                flux(:,j) = LFflux(qL(:,j),qR(:,j),gamma,smax);
            case 'ROE' % Roe
                flux(:,j) = ROEflux(qL(:,j),qR(:,j),gamma);
            case 'RUS' % Rusanov
                flux(:,j) = RUSflux(qL(:,j),qR(:,j),gamma);
            case 'HLLE' % HLLE
                flux(:,j) = HLLEflux(qL(:,j),qR(:,j),gamma);
            case 'AUSM' % AUSM
                flux(:,j) = AUSMflux(qL(:,j),qR(:,j),gamma);
            case 'HLLC' % HLLC
                flux(:,j) = HLLCflux(qL(:,j),qR(:,j),gamma);
        end
        res(:, j ) = res(:, j ) + flux(:,j)/dx;
        res(:,j+1) = res(:,j+1) - flux(:,j)/dx;
    end

    % Flux contribution of the LEFT MOST FACE: left face of cell j=2.
    qR(:,1)=q(:,2)-dq(:,2)*dx/2;    qL(:,1) = qR(:,1);
    % compute: flux(:,1) = AUSMflux(qL(:,1),qR(:,1),gamma);
    switch fluxMethod
        case 'LF' % Lax-Friedrichs
            flux(:,1) = LFflux(qL(:,1),qR(:,1),gamma,smax);
        case 'ROE' % Roe
            flux(:,1) = ROEflux(qL(:,1),qR(:,1),gamma);
        case 'RUS' % Rusanov
            flux(:,1) = RUSflux(qL(:,1),qR(:,1),gamma);
        case 'HLLE' % HLLE
            flux(:,1) = HLLEflux(qL(:,1),qR(:,1),gamma);
        case 'AUSM' % AUSM
            flux(:,1) = AUSMflux(qL(:,1),qR(:,1),gamma);
        case 'HLLC' % HLLC
            flux(:,1) = HLLCflux(qL(:,1),qR(:,1),gamma);
    end
    res(:,2) = res(:,2) - flux(:,1)/dx;

    % Flux contribution of the RIGTH MOST FACE: right face of cell j=N-1.
    qL(:,N-1)=q(:,N-1)+dq(:,N-1)*dx/2;      qR(:,N-1) = qL(:,N-1);
    % compute: flux(:,N-1) = Xflux(qL(:,N-1),qR(:,N-1),gamma);
    switch fluxMethod
        case 'LF' % Lax-Friedrichs
            flux(:,N-1) = LFflux(qL(:,N-1),qR(:,N-1),gamma,smax);
        case 'ROE' % Roe
            flux(:,N-1) = ROEflux(qL(:,N-1),qR(:,N-1),gamma);
        case 'RUS' % Rusanov
            flux(:,N-1) = RUSflux(qL(:,N-1),qR(:,N-1),gamma);
        case 'HLLE' % HLLE
            flux(:,N-1) = HLLEflux(qL(:,N-1),qR(:,N-1),gamma);
        case 'AUSM' % AUSM
            flux(:,N-1) = AUSMflux(qL(:,N-1),qR(:,N-1),gamma);
        case 'HLLC' % HLLC
            flux(:,N-1) = HLLCflux(qL(:,N-1),qR(:,N-1),gamma);
    end
    res(:,N-1) = res(:,N-1) + flux(:,N-1)/dx;
end

function mm = minmod(v)
    % Using Harten's generalized definition
    % minmod: zero if opposite sign, otherwise the one of smaller magnitude.
    %m=size(v,1); mm=zeros(size(v,2),1); s=sum(sign(v),2)/m; ids=find(abs(s)==1);
    %if(~isempty(ids)); mm(ids)=s(ids).*min(abs(v(ids,:)),[],2); end
    s = sum(sign(v))/numel(v); 
    if abs(s)==1; mm = s*min(abs(v(:))); else, mm=0; end
end

function va = vanalbada(da,db,h)
    % Van Albada Slope Limiter Function
    % vanAlbada: extend the simetric formulation of the van leer limiter
    eps2=(0.3*h)^3; 
    va=0.5*(sign(da)*sign(db)+1)*((db^2+eps2)*da+(da^2+eps2)*db)/(da^2+db^2+2*eps2);
end

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

function Roe = ROEflux(qL,qR,gamma)
    % Roe flux function
    %
    
    % Left state
    rL = qL(1);
    uL = qL(2)./rL;
    EL = qL(3)./rL;
    pL = (gamma-1)*( qL(3) - rL*uL*uL/2 );
    aL = sqrt(gamma*pL/rL);
    HL = ( qL(3) + pL )./rL;
    
    % Right state
    rR = qR(1);
    uR = qR(2)./rR;
    ER = qR(3)./rR;
    pR = (gamma-1)*( qR(3) - rR*uR*uR/2 );
    aR = sqrt(gamma*pR/rR);
    HR = ( qR(3) + pR )./rR;
    
    % First compute the Roe Averages
    RT = sqrt(rR/rL);
    r = RT*rL;
    u = (uL+RT*uR)/(1+RT);
    H = (HL+RT*HR)/(1+RT);
    a = sqrt( (gamma-1)*(H-u*u/2) );
    
    % Differences in primitive variables.
    dr = rR - rL;
    du = uR - uL;
    dP = pR - pL;
    
    % Wave strength (Characteristic Variables).
    dV = [(dP-r*a*du)/(2*a^2); -( dP/(a^2)-dr); (dP+r*a*du)/(2*a^2)];
    
    % Absolute values of the wave speeds (Eigenvalues)
    ws = [ abs(u-a); abs( u ); abs(u+a) ];

    % Harten's Entropy Fix JCP(1983), 49, pp357-393.
    % There are various ways to implement the entropy fix. This is just one
    % example. Try turn this off. The solution may be more accurate.
    Da = max(0,4*((uR-aR)-(uL-aL))); if (ws(1)<Da/2); ws(1)=ws(1)*ws(1)/Da+Da/4; end
    Da = max(0,4*((uR+aR)-(uL+aL))); if (ws(3)<Da/2); ws(3)=ws(3)*ws(3)/Da+Da/4; end

    % Right eigenvectors
    R = [  1  ,  1  ,  1  ;
         u-a ,  u  , u+a ;
        H-u*a,u^2/2,H+u*a];
   
    % Compute the average flux.
    FL=[rL.*uL; rL.*uL.^2+pL; uL.*(rL.*EL+pL)];
    FR=[rR.*uR; rR.*uR.^2+pR; uR.*(rR.*ER+pR)];

    % Add the matrix dissipation term to complete the Roe flux.
    Roe = ( FL + FR  - R*(ws.*dV))/2;
end

function Rusanov = RUSflux(qL,qR,gamma)
    % Rusanov flux 
    
    % Left state
    rL = qL(1);
    uL = qL(2)./qL(1);
    EL = qL(3)./rL;
    pL = (gamma-1)*( qL(3) - rL*uL*uL/2 );
    HL = ( qL(3) + pL )./ rL;
    
    % Right state
    rR = qR(1);
    uR = qR(2)./qR(1);
    ER = qR(3)./rR;
    pR = (gamma-1)*( qR(3) - rR*uR*uR/2 );
    HR = ( qR(3) + pR )./ rR;
    
    % First compute the Roe Averages
    RT = sqrt(rR/rL);
    %r = RT*rL;
    u = (uL+RT*uR)/(1+RT);
    H = (HL+RT*HR)/(1+RT);
    a = sqrt( (gamma-1)*(H-u*u/2) );
    
    % Left and Right fluxes
    FL=[rL.*uL; rL.*uL.^2+pL; uL.*(rL.*EL+pL)];
    FR=[rR.*uR; rR.*uR.^2+pR; uR.*(rR.*ER+pR)];
    
    % Rusanov numerical flux
    smax = abs(u)+a;     Rusanov = ( FR + FL + smax*(qL-qR) )/2;
end

function AUSM = AUSMflux(qL,qR,gamma)
    % AUSM numerical flux
    %
    % M.-S. Liou and C. J. Steffen, A New Flux Splitting Scheme, Journal of
    % Computational Physics, 107, pp. 23-39, 1993.

    % Left state
    rL = qL(1);
    uL = qL(2)./qL(1);
    pL = (gamma-1)*( qL(3) - rL*uL*uL/2 );
    aL = sqrt(gamma*pL./rL);
    ML = uL/aL;
    HL = ( qL(3) + pL )./ rL;
    
    % Right state
    rR = qR(1);
    uR = qR(2)./qR(1);
    pR = (gamma-1)*( qR(3) - rR*uR*uR/2 );
    aR = sqrt(gamma*pR./rR);
    MR = uR/aR;
    HR = ( qR(3) + pR )./ rR;

    % Positive M and p in the LEFT cell.
    if (ML <= -1)
        Mp = 0;
        Pp = 0;
    elseif (ML < 1)
        Mp = (ML+1)*(ML+1)/4;
        Pp = pL*(1+ML)*(1+ML)*(2-ML)/4; % or use Pp = (1+ML)*pL/2
    else
        Mp = ML;
        Pp = pL;
    end

    % Negative M and p in the RIGHT cell.
    if   (MR <= -1)
        Mm = MR;
        Pm = pR;
    elseif (MR < 1)
        Mm = -(MR-1)*(MR-1)/4;
        Pm =  pR*(1-MR)*(1-MR)*(2+MR)/4; % or use Pm = (1-MR)*pR/2
    else
        Mm = 0;
        Pm = 0;
    end

    % Positive Part of Flux evaluated in the left cell.
    Fp(1) = max(0,Mp+Mm)*aL * rL;
    Fp(2) = max(0,Mp+Mm)*aL * rL*uL  + Pp;
    Fp(3) = max(0,Mp+Mm)*aL * rL*HL;

    % Negative Part of Flux evaluated in the right cell.
    Fm(1) = min(0,Mp+Mm)*aR * rR;
    Fm(2) = min(0,Mp+Mm)*aR * rR*uR  + Pm;
    Fm(3) = min(0,Mp+Mm)*aR * rR*HR;

    % Compute the flux: Fp(uL)+Fm(uR).
    AUSM = Fp + Fm;
end

function HLLE = HLLEflux(qL,qR,gamma)
    % Compute HLLE flux

    % Left state
    rL = qL(1);
    uL = qL(2)./rL;
    EL = qL(3)./rL;
    pL = (gamma-1)*( qL(3) - rL*uL*uL/2 );
    aL = sqrt(gamma*pL/rL);
    HL = ( qL(3) + pL )./ rL;
    
    % Right state
    rR = qR(1);
    uR = qR(2)./rR;
    ER = qR(3)./rR;
    pR = (gamma-1)*( qR(3) - rR*uR*uR/2 );
    aR = sqrt(gamma*pR/rR);
    HR = ( qR(3) + pR )./ rR;
    
    % Evaluate the two wave speeds: Einfeldt.
    RT = sqrt(rR/rL);
    u = (uL+RT*uR)/(1+RT);
    H = (HL+RT*HR)/(1+RT);
    a = sqrt( (gamma-1)*(H-u*u/2) );
    
    % Wave speed estimates
    SLm = min(uL-aL, u-a);
    SRp = max(uR+aR, u+a);
    
    % Left and Right fluxes
    FL=[rL.*uL; rL.*uL.^2+pL; uL.*(rL.*EL+pL)];
    FR=[rR.*uR; rR.*uR.^2+pR; uR.*(rR.*ER+pR)];
    
    % Compute the HLL flux.
    if SLm >= 0  % Right-going supersonic flow
        HLLE = FL;
    elseif (SLm <= 0) && (SRp >= 0) % Subsonic flow
        select = 1;
        switch select
            case 1 % True HLLE function
                HLLE = ( SRp*FL - SLm*FR + SLm*SRp*(qR-qL) )/(SRp-SLm);
            case 2 % Rusanov flux ( as suggested by Toro's book )
                smax = max(abs(SLm),abs(SRp)); 
                HLLE = ( FR + FL + smax*(qL-qR) )/2; % Rusanov flux%
        end
    elseif  SRp <= 0 % Left-going supersonic flow
        HLLE = FR;
    end
end

function HLLC = HLLCflux(qL,qR,gamma)
    % Compute HLLC flux

    % Left state
    rL = qL(1);
    uL = qL(2)./rL;
    EL = qL(3)./rL;
    pL = (gamma-1)*( qL(3) - rL*uL*uL/2 );
    aL = sqrt(gamma*pL/rL);
    
    % Right state
    rR = qR(1);
    uR = qR(2)./rR;
    ER = qR(3)./rR;
    pR = (gamma-1)*( qR(3) - rR*uR*uR/2 );
    aR = sqrt(gamma*pR/rR);
    
    % Left and Right fluxes
    FL=[rL.*uL; rL.*uL.^2+pL; uL.*(rL.*EL+pL)];
    FR=[rR.*uR; rR.*uR.^2+pR; uR.*(rR.*ER+pR)];

    % Compute guess pressure from PVRS Riemann solver
    PPV  = max(0 , 0.5*(pL+pR) + 0.5*(uL-uR) * (0.25*(rL+rR)*(aL+aR)));
    pmin = min(pL,pR);
    pmax = max(pL,pR);
    Qmax = pmax/pmin;
    Quser= 2.0; % <--- parameter manually set (I don't like this!)
    
     if (Qmax <= Quser) && (pmin <= PPV) && (PPV <= pmax)
     % Select PRVS Riemann solver
         pM = PPV;
         %uM = 0.5*(uL + uR) + 0.5*(pL - pR)/CUP;
      else
         if PPV < pmin
         % Select Two-Rarefaction Riemann solver
            PQ  = (pL/pR)^(gamma - 1.0)/(2.0*gamma);
            uM  = (PQ*uL/aL + uR/aR + 2/(gamma-1)*(PQ-1.0))/(PQ/aL+1.0/aR);
            PTL = 1 + (gamma-1)/2.0*(uL - uM)/aL;
            PTR = 1 + (gamma-1)/2.0*(uM - uR)/aR;
            pM  = 0.5*(pL*PTL^(2*gamma/(gamma-1)) + pR*PTR^(2*gamma/(gamma-1)));
         else 
         % Use Two-Shock Riemann solver with PVRS as estimate
            GEL = sqrt((2/(gamma+1)/rL)/((gamma-1)/(gamma+1)*pL + PPV));
            GER = sqrt((2/(gamma+1)/rR)/((gamma-1)/(gamma+1)*pR + PPV));
            pM  = (GEL*pL + GER*pR - (uR - uL))/(GEL + GER);
            %uM  = 0.5*(uL + uR) + 0.5*(GER*(pM - pR) - GEL*(pM - pL));
         end
      end

    % Estimate wave speeds: SL, SR and SM (Toro, 1994)
    if pM>pL; zL=sqrt(1+(gamma+1)/(2*gamma)*(pM/pL-1)); else, zL=1; end    
    if pM>pR; zR=sqrt(1+(gamma+1)/(2*gamma)*(pM/pR-1)); else, zR=1; end
  
	SL = uL - aL*zL;
    SR = uR + aR*zR;
    SM = (pL-pR + rR*uR*(SR-uR) - rL*uL*(SL-uL))/(rR*(SR-uR) - rL*(SL-uL));
    
    % Compute the HLL flux.
    if 0 <= SL  % Right-going supersonic flow
        HLLC = FL;
    elseif (SL <= 0) && (0 <= SM)	% Subsonic flow to the right
        qsL = rL*(SL-uL)/(SL-SM)*[1; SM; qL(3)/rL + (SM-uL)*(SM+pL/(rL*(SL-uL)))];
        HLLC = FL + SL*(qsL - qL);
    elseif (SM <= 0) && (0 <= SR)	% Subsonic flow to the Left
        qsR = rR*(SR-uR)/(SR-SM)*[1; SM; qR(3)/rR + (SM-uR)*(SM+pR/(rR*(SR-uR)))];
        HLLC = FR + SR*(qsR - qR);
    elseif  0 >= SR % Left-going supersonic flow
        HLLC = FR;
    end
end