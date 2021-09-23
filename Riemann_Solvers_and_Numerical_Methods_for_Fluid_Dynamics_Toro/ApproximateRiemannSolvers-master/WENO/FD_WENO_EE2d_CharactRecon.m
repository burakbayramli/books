function res = FD_WENO_EE2d_CharactRecon(q,a,nx,ny,dx,dy,t,fsplitMth,Recon,Test)
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
        case {'WENO5'}, R=3; % R: stencil size and number of gost cells
        case {'WENO7'}, R=4;
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
            for j=R+1:ny-R
                for i=1:R
                    q(j,i,:)=q(j,R+1,:); q(j,nx+1-i,:)=q(j,nx-R,:);	% Neumann BCs
                    %q(j,i,:)=postshock; q(j,nx+1-i,:)=preshock;	% Dirichlet BCs
                end
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
                    if distance_to_shock(i*dx+dx/2,(j+R)*dy+dy/2,t) < 0 % mesh_shock
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
        case 'RUS', [fp,fm] = Rusanov(q(ic,:,:),[1,0]); % Rusanov (Rus) Flux Splitting
        otherwise, error('Splitting method not set.');
    end

% 3. Reconstruct interface values: qL=q_{i+1/2}^{-} and qR=q_{i-1/2}^{+}
    E=4; % numer of components or layers
    switch Recon
        case 'WENO5', [flux] = WENO5charWiseRecon_X(q(ic,:,:),fp,fm,nx);
        %case 'WENO7', [flux] = WENO7charWiseRecon_X(q(ic,:,:),fp,fm,nx);
        otherwise, error('reconstruction not available ;P');
    end

% 4. Compute finite volume residual term, df/dx.
    res=zeros(size(q)); nc=ny-2*R; nf=nx+1-2*R;

    % Flux contribution to the residual of every cell
    for e=1:E
        for j=1:nc % for all interior cells
            res(j+R,R+1,e) = res(j+R,R+1,e) - flux(j,1,e)/dx; % left face of cell j=4.
            for i = 2:nf-1 % for all interior faces
                res(j+R,i+R-1,e) = res(j+R,i+R-1,e) + flux(j,i,e)/dx;
                res(j+R, i+R ,e) = res(j+R, i+R ,e) - flux(j,i,e)/dx;
            end
            res(j+R,nx-R,e) = res(j+R,nx-R,e) + flux(j,nc,e)/dx; % right face of cell j=N-3.
        end
    end
    
% 5. Produce flux splitting in y-direction

	% Clear flux variables
    clear flux fp fm;

    % we only consider internal cells
    ic=R+1:nx-R;
    switch fsplitMth
        case 'LF',  [fp,fm] = LF(a,q(:,ic,:),[0,1]);    % Lax-Friedrichs (LF) Flux Splitting
        case 'RUS', [fp,fm] = Rusanov(q(:,ic,:),[0,1]); % Rusanov (Rus) Flux Splitting
        otherwise, error('Splitting method not set.');
    end

% 6. Reconstruct interface values: qL=q_{j+1/2}^{-} and qR=q_{j-1/2}^{+}
    switch Recon
        case 'WENO5', [flux] = WENO5charWiseRecon_Y(q(:,ic,:),fp,fm,ny);
        %case 'WENO7', [flux] = WENO7charWiseRecon_Y(q(:,ic,:),fp,fm,ny);
    end

% 7. Compute finite volume residual term, dg/dy.
    nc=nx-2*R; nf=ny+1-2*R;
    
    % Flux contribution to the residual of every cell
    for e=1:E
        for i=1:nc % for all interior cells
            res(R+1,i+R,e) = res(R+1,i+R,e) - flux(1,i,e)/dy;
            for j=2:nf-1 % for all interior cells
                res(j+R-1,i+R,e) = res(j+R-1,i+R,e) + flux(j,i,e)/dy;
                res( j+R ,i+R,e) = res( j+R ,i+R,e) - flux(j,i,e)/dy;
            end
            res(ny-R,i+R,e) = res(ny-R,i+R,e) + flux(nf,i,e)/dy;
        end
    end

end % FDM WENO

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

%%%%%%%%%%%%%%%%%%%%%%%
% WENO reconstructions
%%%%%%%%%%%%%%%%%%%%%%%

function [flux] = WENO5charWiseRecon_X(q,fp,fm,N)
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

% R: substencil size, EE: components, nf: total number of internal faces;
R=3; EE=4; I=R:N-R; % nf=N+1-2*R; % All internal faces

% Reconstruction parameters
epweno=1E-40; gamma1=gamma-1;

% Compute flux differences for the entire domain
dfp = fp(:,2:N,:)-fp(:,1:N-1,:); % df{+}_{j+1/2}
dfm = fm(:,2:N,:)-fm(:,1:N-1,:); % df{-}_{j+1/2}
    
% Compute the part of the reconstruction that is stencil-independent
f=fp+fm; flux=(-f(:,I-1,:)+7*(f(:,I,:)+f(:,I+1,:))-f(:,I+2,:))/12; % f_{j+1/2}

% Compute eigenvectors at the cell interfaces j+1/2
for j = 1:size(q,1)
    for i = I % all internal faces of the domain

        % 1. Using simple mean to compute cell interface properties
        r = (q(j,i,1)+q(j,i+1,1))/2;
        u = (q(j,i,2)+q(j,i+1,2))/(2*r);
        v = (q(j,i,3)+q(j,i+1,3))/(2*r);
        E = (q(j,i,4)+q(j,i+1,4))/2;
        U = 0.5*(u^2+v^2);
        p = gamma1*(E-r*U);
        H = (E+p)/r;
        c2 = gamma1*(H-U);
        c = sqrt(gamma*p/r);

        % 2. Compute eigenvectors at the cell interface

        % Construct matrix of right eigenvectors
        %      _                     _ 
        %     |                       |
        %     |   1     1    0    1   |
        %     |                       |
        % R = |  u-c    u    0   u+c  |
        %     |                       |
        %     |   v     v    1    v   |
        %     |                       |
        %     |  H-uc   q    v   H+uc |
        %     |_                     _|
        %
        % where q = 0.5*(u^2+v^2) 

        evr = [...
              1  , 1 , 0 ,  1  ;...
             u-c , u , 0 , u+c ;...
              v  , v , 1 ,  v  ;...
            H-u*c, U , v ,H+u*c];

        % Construct matrix of left eigenvectors
        %         _                                        _ 
        %        |                                          |
        %        | (g-1)*q+c*u  -(g-1)*u-c  -(g-1)*v  (g-1) |
        %        |  ----------   ---------   -------  ----- |
        %        |    2*c^2        2*c^2       2*c^2  2*c^2 |
        %        |                                          |
        % R^{-1}=| c^2-(g-1)*q    (g-1)*u    (g-1)*v -(g-1) |
        %        |  ----------    -------    -------  ----- |
        %        |      c^2         c^2        c^2     c^2  |
        %        |                                          |
        %        |      -v          0          1       0    |
        %        |                                          |
        %        | (g-1)*q-c*u  -(g-1)*u+c  -(g-1)*v  (g-1) |
        %        |  ----------   ---------   -------  ----- |
        %        |    2*c^2        2*c^2       2*c^2  2*c^2 |
        %        |_                                        _|
        %
        % where q = 0.5*(u^2+v^2) 

        evl = [...
             (U*gamma1+c*u)/(2*c2),-(c+u*gamma1)/(2*c2),-(v*gamma1)/(2*c2), gamma1/(2*c2);...
               (c2-U*gamma1)/c2   ,   (u*gamma1)/c2    , (v*gamma1)/c2    ,-(gamma1)/c2  ;...
                    -v            ,         0          ,         1        ,        0     ;...
             (U*gamma1-c*u)/(2*c2), (c-u*gamma1)/(2*c2),-(v*gamma1)/(2*c2), gamma1/(2*c2)];

        % 3. Compute the nonlinear part of the reconstruction

        % Project the splitted flux jumps to the right eigenvector space
        dfps=evl*squeeze(dfp(j,-2+i:i+1,:))';
        dfms=evl*squeeze(dfm(j,-1+i:i+2,:))';

        for idx=1:2
            
            im=(-1)^(idx+1); i1=im+R; in1=-im+R; in2=-2*im+R;
            % idx=1: (reconstruct qL) [in2,in1,R,i1]=[1,2,3,4];
            % idx=2: (reconstruct qR) [in2,in1,R,i1]=[5,4,3,2];
            
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
        h=evr*(s1.*(AmB-BmC)+(0.5*s3-0.25).*(BmC-CmD))/3;
        for e=1:EE
            flux(j,i+1-R,e) = flux(j,i+1-R,e) - h(e);
        end

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
        h = evr*(s1.*(AmB-BmC)+(0.5*s3-0.25).*(BmC-CmD))/3;
        for e=1:EE
            flux(j,i+1-R,e) = flux(j,i+1-R,e) + h(e);
        end

    end % loop over each interface
end 
end

function [flux] = WENO5charWiseRecon_Y(q,gp,gm,N)
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

% R: substencil size, EE: components, nf: total number of internal faces;
R=3; EE=4; I=R:N-R; % nf=N+1-2*R; % All internal faces

% Reconstruction parameters
epweno=1E-40; gamma1=gamma-1;

% Compute flux differences for the entire domain
dgp = gp(2:N,:,:)-gp(1:N-1,:,:); % df{+}_{j+1/2}
dgm = gm(2:N,:,:)-gm(1:N-1,:,:); % df{-}_{j+1/2}
    
% Compute the part of the reconstruction that is stencil-independent
g=gp+gm; flux=(-g(I-1,:,:)+7*(g(I,:,:)+g(I+1,:,:))-g(I+2,:,:))/12; % f_{j+1/2}

% Compute eigenvectors at the cell interfaces j+1/2
for j =1:size(q,2)
    for i = I % all internal faces of the domain

        % 1. Using simple mean to compute cell interface properties
        r = (q(i,j,1)+q(i+1,j,1))/2;
        u = (q(i,j,2)+q(i+1,j,2))/(2*r);
        v = (q(i,j,3)+q(i+1,j,3))/(2*r);
        E = (q(i,j,4)+q(i+1,j,4))/2;
        U = 0.5*(u^2+v^2);
        p = gamma1*(E-r*U);
        H = (E+p)/r;
        c2 = gamma1*(H-U);
        c = sqrt(gamma*p/r);

        % 2. Compute eigenvectors at the cell interface

        % Construct matrix of right eigenvectors
        %      _                    _ 
        %     |                      |
        %     |   1    0   1    1    |
        %     |                      |
        %     |   u    1   u    u    |
        %     |                      |
        % R = |  v-c   0   v   v+c   |
        %     |                      |
        %     |  H-vc  u   q   H+vc  |
        %     |_                    _|
        %
        % where q = 0.5*(u^2+v^2) 

        evr = [...
              1  , 0 , 1 ,  1   ;...
              u  , 1 , u ,  u   ;...
             v-c , 0 , v , v+c  ;...
            H-v*c, u , U ,H+v*c];

        % Construct matrix of left eigenvectors
        %         _                                        _ 
        %        |                                          |
        %        | (g-1)*q+c*v  -(g-1)*u  -(g-1)*v-c  (g-1) |
        %        |  ----------   -------   ---------  ----- |
        %        |    2*c^2       2*c^2      2*c^2    2*c^2 |
        %        |                                          |
        % R^{-1}=|      -u          1          0       0    |
        %        |                                          |
        %        | c^2-(g-1)*q   (g-1)*u    (g-1)*v  -(g-1) |
        %        |  ----------   -------    -------   ----- |
        %        |      c^2        c^2        c^2      c^2  |
        %        |                                          |
        %        | (g-1)*q-c*v  -(g-1)*u  -(g-1)*v+c  (g-1) |
        %        |  ----------   -------   ---------  ----- |
        %        |    2*c^2       2*c^2      2*c^2    2*c^2 |
        %        |_                                        _|
        %
        % where q = 0.5*(u^2+v^2) 

        evl = [...
             (U*gamma1+c*v)/(2*c2),-(u*gamma1)/(2*c2),-(c+v*gamma1)/(2*c2), gamma1/(2*c2);...
                    -u            ,         1        ,         0          ,        0     ;...
               (c2-U*gamma1)/c2   ,   (u*gamma1)/c2  ,   (v*gamma1)/c2    ,-(gamma1)/c2  ;...
             (U*gamma1-c*v)/(2*c2),-(u*gamma1)/(2*c2), (c-v*gamma1)/(2*c2), gamma1/(2*c2)];

        % 3. Compute the nonlinear part of the reconstruction

        % Project the splitted flux jumps to the right eigenvector space
        dgps=evl*squeeze(dgp(-2+i:i+1,j,:))';
        dgms=evl*squeeze(dgm(-1+i:i+2,j,:))';

        % Extrapolation $v_{i+1/2}^{-}$ == $f_{i+1/2}^{+}$
        AmB=(dgps(:,1)-dgps(:,2));
        BmC=(dgps(:,2)-dgps(:,3));
        CmD=(dgps(:,3)-dgps(:,4));

        IS1=13*AmB.^2+3*(  dgps(:,1)-3*dgps(:,2)).^2;
        IS2=13*BmC.^2+3*(  dgps(:,2)+  dgps(:,3)).^2;
        IS3=13*CmD.^2+3*(3*dgps(:,3)-  dgps(:,4)).^2;

        IS1=(epweno+IS1).^2;
        IS2=(epweno+IS2).^2;
        IS3=(epweno+IS3).^2;
        s1=IS2.*IS3; s2=6*IS1.*IS3; s3=3*IS1.*IS2;
        ts0=1./(s1+s2+s3); s1=s1.*ts0; s3=s3.*ts0;
        % flux contribution from $f_{i+1/2}^{+}$ reconstruction
        h=evr*(s1.*(AmB-BmC)+(0.5*s3-0.25).*(BmC-CmD))/3;
        for e=1:EE
            flux(i+1-R,j,e) = flux(i+1-R,j,e) - h(e);
            out=isreal(flux(i+1-R,j,e)); if ~out, disp([i,j,e]); end
        end

        % Extrapolation $u_{i+1/2}^{+}$ == $f_{i+1/2}^{-}$
        AmB=(dgms(:,4)-dgms(:,3));
        BmC=(dgms(:,3)-dgms(:,2));
        CmD=(dgms(:,2)-dgms(:,1));

        IS1=13*AmB.^2+3*(  dgms(:,4)-3*dgms(:,3)).^2;
        IS2=13*BmC.^2+3*(  dgms(:,3)+  dgms(:,2)).^2;
        IS3=13*CmD.^2+3*(3*dgms(:,2)-  dgms(:,1)).^2;

        IS1=(epweno+IS1).^2;
        IS2=(epweno+IS2).^2;
        IS3=(epweno+IS3).^2;
        s1=IS2.*IS3; s2=6*IS1.*IS3; s3=3*IS1.*IS2;
        ts0=1./(s1+s2+s3); s1=s1.*ts0; s3=s3.*ts0;
        % flux contribution from $f_{i+1/2}^{-}$ reconstruction
        h=evr*(s1.*(AmB-BmC)+(0.5*s3-0.25).*(BmC-CmD))/3;
        for e=1:EE
            flux(i+1-R,j,e) = flux(i+1-R,j,e) + h(e);
            out=isreal(flux(i+1-R,j,e)); if ~out, disp([i,j,e]); end
        end
    end % loop over each interface
end
end