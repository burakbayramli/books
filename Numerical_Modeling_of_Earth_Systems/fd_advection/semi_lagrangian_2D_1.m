% semi_lagrangian_2D: 2D semi-lagrangian  with center midpoint time stepping method
%

clear all

W       =   40;     % width of domain
sigma   =   .1;
Ampl    =   2;
nt      =   500;    % number of timesteps
dt      =   5e-1;

% Initial grid and velocity
[x,z]   =   meshgrid(-0.5:.025:0.5,-0.5:.025:0.5);
nz      =   size(x,1);
nx      =   size(x,2);

% Initial gaussian T-profile
T       =   Ampl*exp(-((x+0.25).^2+z.^2)/sigma^2);

% Velocity
Vx      =   z;
Vz      =   -x;

for itime=1:nt

    Vx_n    = Vx;                   %   Velocity at time=n
    Vx_n1   = Vx;                   %   Velocity at time=n+1
%    Vx_n1_2 = ??;                   %   Velocity at time=n+1/2
%    Vz_n    = ??;                   %   Velocity at time=n
%    Vz_n1   = ??;                   %   Velocity at time=n+1
%    Vz_n1_2 = ??;     %   Velocity at time=n+1/2
    Tnew    =   zeros(size(T));
    for ix=2:nx-1
        for iz=2:nz-1

            Vx_cen = Vx(iz,ix);
            Vz_cen = Vz(iz,ix);
   %         for ??
%                X =?
%                Z = ?

                %linear interpolation of velocity
%                Vx_cen = interp2(x,z,?,?, ?, 'linear');
%                Vz_cen = interp2(x,z,?,?, ?, 'linear');

                if isnan(Vx_cen)
                    Vx_cen =   Vx(iz,ix);
                end
                if isnan(Vz_cen)
                    Vz_cen =   Vz(iz,ix);
                end


%            end
%            X = ?;
%            Z = ?;
            
            % Interpolate temperature on X
%            T_X = interp2(x,z,?,?,?, 'cubic');
            if isnan(T_X)
                T_X = T(iz,ix);
            end

            Tnew(iz,ix) = T_X;
        end

    end

    Tnew(1,:)   =   T(1,:);
    Tnew(nx,:)  =   T(nx,:);
    Tnew(:,1)   =   T(:,1);
    Tnew(:,nx)  =   T(:,nx);

    T           =   Tnew;
    time        =   itime*dt;


    figure(1),clf
    pcolor(x,z,T), shading interp, hold on, colorbar
    contour(x,z,T,[.1:.1:2],'k'), 
    hold on, quiver(x,z,Vx,Vz,'w')
    axis equal, axis tight
    drawnow
    pause
end
