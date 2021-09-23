function res = FV_WENO_EE1d_charWiseRecon(q,a,nx,dx,fluxMethod,Recon,test)
% *************************************************************************
%
%          Finite Volume solver for system of equations
%
% Coded by Manuel A. Diaz, 02.10.2012, NTU Taiwan.
% Last update on 2016.04.29, NHRI Taiwan.
% *************************************************************************
% Compute RHS of the semi-discrete form of the Euler equations.
%
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
% *************************************************************************

%% 1. Set boundary conditions for Riemann Problems: out flux BCs

    % 1.1 Identify number of gost cells
    switch Recon
        case {'WENO5','Poly5'}, R=3; % R: stencil size and number of gost cells
        case {'WENO7','Poly7'}, R=4;
        otherwise, error('reconstruction not available ;P');
    end
    
    % 1.2 Set Left and right boundary conditions
    switch test
        case 'Riemann'
            for i=1:R
                q(:,i)=q(:,R+1); q(:,nx+1-i)=q(:,nx-R);	% Neumann BCs
            end
        case 'CWblastwave'
            for i=1:R
                q(1,i)= q(1,R+1); q(1,nx+1-i)= q(1,nx-R);	
                q(2,i)=-q(2,R+1); q(2,nx+1-i)=-q(2,nx-R); % Reflective BCs
                q(3,i)= q(3,R+1); q(3,nx+1-i)= q(3,nx-R);
            end
        otherwise, error('BCs for test not set!');
    end

% 2. Reconstruct interface values: qL=q_{i+1/2}^{-} and qR=q_{i-1/2}^{+}

    % Produce reconstruction
    switch Recon
        case 'WENO5', [qL,qR] = WENO5charWiseRecon(q,nx);
        %case 'WENO7', [qL,qR] = WENO7charWiseRecon(q,nx);
        %case 'Poly5', [qL,qR] = POLY5charWiseRecon(q,nx);
        %case 'Poly7', [qL,qR] = POLY7charWiseRecon(q,nx);
        otherwise, error('reconstruction not available ;P');
    end

% 3. Compute finite volume residual term, df/dx.
res=zeros(size(q)); flux=zeros(size(qR)); nf=nx+1-2*R;

    % compute flux at j+1/2
    for j = 1:nf % for all interior faces 
        switch fluxMethod
            case 'LF',  flux(:,j) = LFflux(qL(:,j),qR(:,j),a); % LF
            case 'ROE', flux(:,j) = ROEflux(qL(:,j),qR(:,j));  % Roe
            case 'LLF', flux(:,j) = RUSflux(qL(:,j),qR(:,j));  % Rusanov
            case 'HLLE',flux(:,j) = HLLEflux(qL(:,j),qR(:,j)); % HLLE
            case 'AUSM',flux(:,j) = AUSMflux(qL(:,j),qR(:,j)); % AUSM
            case 'HLLC',flux(:,j) = HLLCflux(qL(:,j),qR(:,j)); % HLLC
        end
    end

    % Flux contribution to the residual of every cell
    res(:,R+1) = res(:,R+1) - flux(:,1)/dx; % left face of cell j=4.
    for j = 2:nf-1 % for all interior faces 
        res(:,j+R-1) = res(:,j+R-1) + flux(:,j)/dx;
        res(:, j+R ) = res(:, j+R ) - flux(:,j)/dx;
    end
    res(:,nx-R)= res(:,nx-R)+ flux(:,nf)/dx; % right face of cell j=N-3.

end % FVM WENO

%%%%%%%%%%%%%%%%%%
% Flux Functions
%%%%%%%%%%%%%%%%%%

function FL = LFflux(qL,qR,smax)
    % Lax-Friedrichs flux:
    %
    % P. D. Lax, Weak Solutions of Nonlinear Hyperbolic Equations and Their
    % Numerical Computation, Commun. Pure and Applied Mathematics, 7, 159-193, 
    % 1954.
    global gamma
    
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

function Roe = ROEflux(qL,qR)
    % Roe flux function
    global gamma
    
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

function Rusanov = RUSflux(qL,qR)
    % Rusanov flux 
    global gamma
    
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

function AUSM = AUSMflux(qL,qR)
    % AUSM numerical flux
    %
    % M.-S. Liou and C. J. Steffen, A New Flux Splitting Scheme, Journal of
    % Computational Physics, 107, pp. 23-39, 1993.
    global gamma

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

function HLLE = HLLEflux(qL,qR)
    % Compute HLLE flux
    global gamma

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
    if (0 <= SLm)  % Right-going supersonic flow
        HLLE = FL;
    elseif (SLm <= 0) && (0 <= SRp) % Subsonic flow
        select = 1;
        switch select
            case 1 % True HLLE function
                HLLE = ( SRp*FL - SLm*FR + SLm*SRp*(qR-qL) )/(SRp-SLm);
            case 2 % Rusanov flux ( as suggested by Toro's book )
                smax = max(abs(SLm),abs(SRp)); 
                HLLE = ( FR + FL + smax*(qL-qR) )/2; % Rusanov flux%
        end
    elseif  (0 >= SRp) % Left-going supersonic flow
        HLLE = FR;
    end
end

function HLLC = HLLCflux(qL,qR)
    % Compute HLLC flux
    global gamma

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

%%%%%%%%%%%%%%%%%%
% Reconstructions
%%%%%%%%%%%%%%%%%%

function [qL,qR] = WENO5charWiseRecon(q,N)
% *************************************************************************
% Based on:
% [1] Jiang, Guang-Shan, and Cheng-chin Wu. "A high-order WENO finite
%     difference scheme for the equations of ideal magnetohydrodynamics."
%     Journal of Computational Physics 150.2 (1999): 561-594.
%
% coded by Manuel Diaz, 02.10.2012, NTU Taiwan.
% last update on 2016.04.29, NHRI Taiwan.
% *************************************************************************
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
global gamma

R=3; E=3; I=R:N-R; % R: substencil size, E: system components;
epweno=1E-40; gamma1=gamma-1;

averageMth='roe';
evr=zeros(3,3,N-1);
evl=zeros(3,3,N-1);
h=zeros(2,N-1);

% compute and store the differences for the entire domain
dq = q(:,2:N)-q(:,1:N-1); % dq_{j+1/2}
    
% Compute the part of the reconstruction that is stencil-independent
qL = (-q(:,I-1)+7*(q(:,I)+q(:,I+1))-q(:,I+2))/12; qR = qL; % dq_{j+1/2}

% Compute eigenvectors at the cell interfaces j+1/2
for i = I
    % 1. Compute averages all cell interfaces
    switch averageMth
        case 'simple' % Use simple averages
            r = (q(1,i)+q(1,i+1))/2;
            u = (q(2,i)+q(2,i+1))/(2*r);
            E = (q(3,i)+q(3,i+1))/2;
            p = gamma1*(E - 0.5*r*u^2);
            H = (E+p)/r;
            c2 = gamma1*(H - 0.5*u^2);
            c = sqrt(c2);
        case 'roe' % Use Roe averages
            r_sqrtl = sqrt(q(1, i ));
            r_sqrtr = sqrt(q(1,i+1));
            pl = gamma1*(q(3, i ) - 0.5*(q(2, i )^2)/q(1, i ));
            pr = gamma1*(q(3,i+1) - 0.5*(q(2,i+1)^2)/q(1,i+1));
            r_sq2 = r_sqrtl + r_sqrtr;
            u = (q(2, i )/r_sqrtl + q(2,i+1)/r_sqrtr)/r_sq2;
            H = (((q(3, i )+pl)/r_sqrtl + (q(3,i+1)+pr)/r_sqrtr))/r_sq2;
            c2 = gamma1*(H - 0.5*u^2);
            c = sqrt(c2);
    end
    
    % 2. Compute properties at cell interfaces using Roe avegares
    
        % Construct matrix of right eigenvectors
        %      _                    _ 
        %     |                      |
        %     |   1      1       1   |
        %     |                      |
        % R = |  u-c     u      u+c  |
        %     |                      |
        %     |  H-uc   u^2/2   H+uc |
        %     |_                    _|

        evr(:,:,i) = [...
              1  ,  1  ,  1   ;...
             u-c ,  u  , u+c  ;...
            H-u*c,u^2/2,H+u*c];

        % Construct matrix of left eigenvectors
        %                          _                                       _ 
        %                         |                                         |
        %                         |  uc/(gamma-1)+u^2/2  -c/(gamma-1)-u   1 |
        %                         |                                         |
        % R^{-1}=(gamma-1)/(2c^2)*|  2(H-u^2)             2u             -2 |
        %                         |                                         |
        %                         | -uc/(gamma-1)+u^2/2   c/(gamma-1)-u   1 |
        %                         |_                                       _|

        evl(:,:,i) = gamma1/(2*c^2)*[...
             c*u/gamma1+u^2/2,-(c/gamma1+u), 1 ;...
                  2*(H-u^2)  ,    2*u      ,-2 ;...
            -c*u/gamma1+u^2/2, c/gamma1-u  , 1];
end 

% 3. Produce the WENO reconstruction
for ip=1:E

    % Project the jumps at faces to the left characteristic space: qs
    for m2 =-2:2
       for i = I
          qs(m2+R,i) = 0;
          for e=1:E      
            qs(m2+R,i) = qs(m2+R,i) + evl(ip,e,i)*dq(e,i+m2);
          end
       end
    end

    % the reconstruction
    for idx=1:2

        % idx=1: construct hn (qL)
        % idx=2: construct hp (qR)

        im=(-1)^(idx+1); i1=im+R; in1=-im+R; in2=-2*im+R;

        for i=I

            AmB=im*(qs(in2,i)-qs(in1,i));
            BmC=im*(qs(in1,i)-qs( R ,i));
            CmD=im*(qs( R ,i)-qs( i1,i));

            IS1=13*AmB^2+3*(  qs(in2,i)-3*qs(in1,i))^2;
            IS2=13*BmC^2+3*(  qs(in1,i)+  qs( R ,i))^2;
            IS3=13*CmD^2+3*(3*qs( R ,i)-  qs( i1,i))^2;

            IS1=(epweno+IS1)^2;
            IS2=(epweno+IS2)^2;
            IS3=(epweno+IS3)^2;
            s1=IS2*IS3; s2=6*IS1*IS3; s3=3*IS1*IS2;
            st0=1/(s1+s2+s3); s1=s1*st0; s3=s3*st0;

            h(idx,i) = (s1*(BmC-AmB)+(0.5*s3-0.25)*(CmD-BmC))/3;

        end % loop over interfaces
    end % loop over each side of interface

    % Project to the physical space:
    for e=1:E
        for i=I
            qL(e,i+1-R) = qL(e,i+1-R) + evr(e,ip,i)*h(1,i);
            qR(e,i+1-R) = qR(e,i+1-R) + evr(e,ip,i)*h(2,i);
        end
    end
end

end