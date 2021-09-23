function res = FD_WENO_EE1d_charWiseRecon(a,q,nx,dx,fsplitMth,Recon,test)
% *************************************************************************
%
%          Vertor Flux Splitting solver for system of equations
%
% Coded by Manuel A. Diaz, 02.10.2012, NTU Taiwan.
% Last update on 2016.04.29, NHRI Taiwan.
% *************************************************************************
% Compute RHS of the semi-discrete form of the Euler equations.
%
%   Flux at j+1/2
% 
%     j+1/2    Cell's grid: (assuming WENO5, R=3)
%   |   |   |                     {x=0}         {x=L}
%   | f+|   |                       |             |
%   |  /|f- |           1   2   3   4   5        N-3 N-2 N-1  N
%   | / |\  |         |-o-|-o-|-o-|-o-|-o-| ... |-o-|-o-|-o-|-o-|---> j
%   |/  | \ |             1   2   3   4   5    N-4 N-3 N-2 N-1  
%   |   |  \|                    {1} {2} {3}  ...  {nf}
%   |   |   |       NC: Here cells 1 to 3 and N-2 to N are ghost cells
%     j  j+1       nodes 4 and N-3, are the real boundaries of the domain.
%
%   q = cat(3, r, ru, rv, E);
%   F = cat(3, ru, ru^2+p, ruv, u(E+p));
%   G = cat(3, rv, ruv, rv^2+p, v(E+p));
% *************************************************************************

% 1. Set boundary conditions for Riemann Problems: out flux BCs
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

% 2. Produce flux splitting 
switch fsplitMth
    case 'LF',  [fp,fm] = LF(a,q);    % Lax-Friedrichs (LF) Flux Splitting
    case 'LLF', [fp,fm] = Rusanov(q); % Local Lax-Friedrichs (LF) Flux Splitting
    case 'SHLL',[fp,fm] = SHLL(q);    % Split HLL (SHLL) flux 
    otherwise, error('Splitting method not set.');
end

% 3. Produce reconstructions
switch Recon
    case 'WENO5', [flux] = WENO5charWiseRecon(q,fp,fm,nx);
    %case 'WENO7', [flux] = WENO7charWiseRecon(q,fp,fm,nx);
    %case 'Poly5', [flux] = POLY5charWiseRecon(q,fp,fm,nx);
    %case 'Poly7', [flux] = POLY7charWiseRecon(q,fp,fm,nx);
    otherwise, error('chacteristic reconstruction not available ;P');
end

% 4. Compute finite difference residual term, df/dx.
nf=nx+1-2*R; res = zeros(size(q));
res(:,R+1) = res(:,R+1) - flux(:,1)/dx; % left face of cell j=4.
for j = 2:nf-1 % for all interior faces
    res(:,j+R-1) = res(:,j+R-1) + flux(:,j)/dx;
    res(:, j+R ) = res(:, j+R ) - flux(:,j)/dx;
end
res(:,nx-R)= res(:,nx-R)+ flux(:,nf)/dx; % right face of cell j=N-3.

end

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Flux splitting functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Lax-Friedrichs
function [Fp,Fm] = LF(a,q)
    global gamma
    
    % primary properties
    rho=q(1,:); u=q(2,:)./rho; E=q(3,:); 
    p=(gamma-1)*(E-0.5*rho.*u.^2);
    
    % flux vector of conserved properties
    F=[rho.*u; rho.*u.^2+p; u.*(E+p)];
    
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

function [flux] = WENO5charWiseRecon(q,fp,fm,N)
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

% R: substencil size, nf: total number of internal faces;
R=3; I=R:N-R; % nf=N+1-2*R; % All internal faces

% Reconstruction parameters
epweno=1E-40; gamma1=gamma-1;

% Cell interface properties method
averageMth='simple';

% Compute flux differences for the entire domain
dfp = fp(:,2:N)-fp(:,1:N-1); % df{+}_{j+1/2}
dfm = fm(:,2:N)-fm(:,1:N-1); % df{-}_{j+1/2}
    
% Compute the part of the reconstruction that is stencil-independent
f=fp+fm; flux=(-f(:,I-1)+7*(f(:,I)+f(:,I+1))-f(:,I+2))/12; % f_{j+1/2}

% Compute eigenvectors at internal cell interfaces j+1/2
for i = I % all internal faces of the domain
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

        evr = [...
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

        evl = gamma1/(2*c^2)*[...
             c*u/gamma1+u^2/2,-(c/gamma1+u), 1 ;...
                  2*(H-u^2)  ,    2*u      ,-2 ;...
            -c*u/gamma1+u^2/2, c/gamma1-u  , 1];

    % 3. Compute the nonlinear part of the reconstruction
    % Project the splitted flux jumps to the right eigenvector space
    dfps=evl*dfp(:,-2+i:i+1);
    dfms=evl*dfm(:,-1+i:i+2);

    % Extrapolation $v_{i+1/2}^{-}$ == $f_{i+1/2}^{+}$
    AmB=(dfps(:,1)-dfps(:,2));
    BmC=(dfps(:,2)-dfps(:,3));
    CmD=(dfps(:,3)-dfps(:,4));

    IS1=13*AmB.^2+3*(  dfps(:,1)-3*dfps(:,2)).^2;
    IS2=13*BmC.^2+3*(  dfps(:,2)+  dfps(:,3)).^2;
    IS3=13*CmD.^2+3*(3*dfps(:,3)-  dfps(:,4)).^2;

    IS1=(epweno+IS1).^2;
    IS2=(epweno+IS2).^2;
    IS3=(epweno+IS3).^2;
    s1=IS2.*IS3; s2=6*IS1.*IS3; s3=3*IS1.*IS2;
    ts0=1./(s1+s2+s3); s1=s1.*ts0; s3=s3.*ts0;
    % flux contribution from $f_{i+1/2}^{+}$ reconstruction
    flux(:,i+1-R) = flux(:,i+1-R) - evr*(s1.*(AmB-BmC)+(0.5*s3-0.25).*(BmC-CmD))/3;

    % Extrapolation $u_{i+1/2}^{+}$ == $f_{i+1/2}^{-}$
    AmB=(dfms(:,4)-dfms(:,3));
    BmC=(dfms(:,3)-dfms(:,2));
    CmD=(dfms(:,2)-dfms(:,1));

    IS1=13*AmB.^2+3*(  dfms(:,4)-3*dfms(:,3)).^2;
    IS2=13*BmC.^2+3*(  dfms(:,3)+  dfms(:,2)).^2;
    IS3=13*CmD.^2+3*(3*dfms(:,2)-  dfms(:,1)).^2;

    IS1=(epweno+IS1).^2;
    IS2=(epweno+IS2).^2;
    IS3=(epweno+IS3).^2;
    s1=IS2.*IS3; s2=6*IS1.*IS3; s3=3*IS1.*IS2;
    ts0=1./(s1+s2+s3); s1=s1.*ts0; s3=s3.*ts0;
    % flux contribution from $f_{i+1/2}^{-}$ reconstruction
    flux(:,i+1-R) = flux(:,i+1-R) + evr*(s1.*(AmB-BmC)+(0.5*s3-0.25).*(BmC-CmD))/3;

end % loop over each interface

end