%
% 2D Elastic wave propagtaion problem
% Staggered grid
%
%     (j+1/2) --- Txz-----uz-----Txz----uz-----Txz
%                  |      |      |      |      |
%                  |      |      |      |      |
%                  |      |      |      |      |
%         (j) --- ux---Txx,Tzz---ux---Txx,Tzz---ux
%                  |      |      |      |      |
%                  |      |      |      |      |
%                  |      |      |      |      |
%     (j-1/2) --- Txz-----uz-----Txz-----uz-----Txz
%                         |      |      |
%                        (i)  (i+1/2) (i+1)
% In the code:
% Txx,Tzz(i,j) = Txx,Tzz(i,j);
% Txz(i,j) = Txz(i+1/2,j+1/2);
% ux(i,j) = ux(i+1/2,j);
% uz(i,j) = uz(i,j+1/2);


clear all;
%Box dimensions
L       =   6000;      %   Width of box    [m]     
H       =   6000;      %   Height of box   [m] 

% Numerical parameters
nx      =   ???;         %   # gridpoints in x-direction
dh      =   L/(nx-1);    %   Spacing of grid
nz      =   floor(H/dh/2)*2+1;         %   # gridpoints in z-direction

%%% choose nt to avoid the reflections from the boundaries
nt      =   300;          %   Number of timesteps to compute


% The phasical properties grid
Vp      =   5000*ones(nx,nz);      %   Compressional wave velocity [m/s]
Vs      =   2000*ones(nx,nz);      %   Shear wave velocity [m/s]
% Add heterogeneity here
% ????
rho     =   2000*ones(nx,nz);      %   Density [kg/m^3]


miu     =   ??;  %   MU
lambda  =   ??;   %   Lambda


[z2d,x2d] = meshgrid(0:dh:H, -L/2:dh:L/2);  % create grid


% Compute stable timestep
dt         = ???;


% Source time function
half_dur = 0.2;                              % Source half duration [s]


% Wavelength
wl = min(min(Vs))*2*half_dur
% Setup initial velocity and stress profile
ux = zeros(nx,nz);
uz = zeros(nx,nz);
Txx = zeros(nx,nz);
Tzz = zeros(nx,nz);
Txz = zeros(nx,nz);

% Source location
src_nx  =   floor(nx/2)+1;      % source node at x-direction
src_nz  =   floor(nz/2)+1;      % source node at z-direction

% Station location
% Add more stations
sta_x = [500 750 1000 1250; ???];
sta_z = [3000 3000 3000 3000 ???];
sta_num = length(sta_x);
sta_nx = floor((sta_x+L/2)/dh)+1;
sta_nz = floor(sta_z/dh)+1;



 
% Bouyancy
b = 1./rho.*ones(nx,nz);

a = dt/dh;

BU = b*a;
LAM = lambda*a;
MU = miu*a;
GAMMA = LAM + 2*MU;


time = 0;
for n=1:nt
    %Add the source term
    if(time<=2*half_dur)
        % P type source
        %Txx(src_nx,src_nz) =  Txx(src_nx,src_nz)+source_time(time,half_dur);
        %Tzz(src_nx,src_nz) =  Tzz(src_nx,src_nz)+source_time(time,half_dur);
        % typically use this one if you want S type source, you can compare
        % with the P type source
        Txz(src_nx,src_nz) =  Txz(src_nx,src_nz)+source_time(time,half_dur);      
    end
    % Update stress from velocity
    for i=2:nx-1
        for j=2:nz-1
            Txx(i,j) = Txx(i,j) + GAMMA(i,j)*(ux(i,j)-ux(i-1,j)) + LAM(i,j)*(uz(i,j)-uz(i,j-1));
            Tzz(i,j) = ?;
            Txz(i,j) = ?;
        end
    end

    % Set boundary conditions
    Txx([1 nx],:)   =  0;
    Txx(:,[1 nz])   =  0;
    Tzz([1 nx],:)   =  0;
    Tzz(:,[1 nz])   =  0;
    Txz([1 nx],:)   =  0;
    Txz(:,[1 nz])   =  0;    
    

    % Update velocity from stress
    for i=2:nx-1
        for j=2:nz-1
            ux(i,j) = ?;
            uz(i,j) = ?;
        end
    end
      
    % Set boundary conditions
    ux([1 nx],:)   =  0;
    ux(:,[1 nz])   =  0;
    uz([1 nx],:)   =  0;
    uz(:,[1 nz])   =  0;    
    % Time info
    time        =   time+dt;

    
    % For pcolor plot
    PP = ux;
    
    % Save the synthetics at station
    time_save(n) = time;
    
    % save the ux at stations
    U_save = ?;
    % save the uz at stations
    V_save = ?;
    
    % Plot solution every 50 timesteps
    if (mod(n,10)==0)
        figure(1), clf
        pcolor(x2d,z2d,PP); shading interp, colorbar
        %caxis([-2E-12 2E-12])
        hold on
        plot(sta_x,sta_z,'r^',x2d(src_nx,src_nz),z2d(src_nx,src_nz),'rp');
        xlabel('x [m]')
        ylabel('z [m]')
        zlabel('Pressure [Pa]')
        title(['Wave propagation after T = ',num2str(time),' sec'])
        axis equal, axis tight
        drawnow
    end
    %pause
end
figure;
% Plot the station records
subplot(2,1,1), plot(time_save,U_save), xlabel('Time [sec]'), ylabel('Raidal Amp'), legend(?????);
subplot(2,1,1), plot(time_save,V_save), xlabel('Time [sec]'), ylabel('Vertical Amp'), legend(?????);