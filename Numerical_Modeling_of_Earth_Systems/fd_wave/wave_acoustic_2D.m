%
% 2D Acoustic wave propagtaion problem
%
clear all;
%Physical parameters
L       =   6000;      %   Width of box    [m]     
H       =   6000;      %   Height of box   [m] 
V0       =   2000;     %   Bulk sound velocity [m/s]


% Numerical parameters
nx      =   ???;         %   # gridpoints in x-direction
dh      =   L/(nx-1);   %   Spacing of grid, same for x- and z- direction
nz      =   floor(H/dh/2)*2+1;         %   # gridpoints in z-direction

%%% choose nt to avoid reflections from the boundaries
nt      =   500;          %   Number of timesteps to compute

[z2d,x2d] = meshgrid(0:dh:H, -L/2:dh:L/2);  % create grid

% The velocity grid
V         = V0*ones(nx,nz);
% Add heterogeneties here
% ?????

% Compute stable timestep
dt         = ???

% Source time function
half_dur = 0.2;                              % Source half duration [s]

% Wavelength
wl = min(min(V))*2*half_dur

% Setup initial pressure profile
P_pre = zeros(nx,nz);
P_cur = zeros(nx,nz);


% Station location
% You can add more stations here
sta_x = [1000 1500 2000 2500 ...???];
sta_z = [3000 3000 3000 3000 ...???];
sta_num = length(sta_x);
sta_nx = floor((sta_x+L/2)/dh)+1;
sta_nz = floor(sta_z/dh)+1;

% source position
src_nx  =   floor(nx/2)+1;      % source node at x-direction
src_nz  =   floor(nz/2)+1;      % source node at z-direction
    
a   = V.^2*dt^2/dh^2;
time    =   0;
for n=1:nt

    %Add the source term, call function "source_time"
    %for a gaussian shape source

        ?? = ?? + source_time(time,half_dur);

    
    % Compute new pressure

    for i=2:nx-1
        for j=2:nz-1
            P_next(i,j) = ????
        end
    end
    
    
    % Set boundary conditions
    P_next([1 nx],:)   =  0;
    P_next(:,[1 nz])  =  0;
    
    % Update the pressure at previous and current step
    P_pre = ?
    P_cur = ?
    
    % Time info
    time        =   time+dt;


    

    
    % P for plot
    P           =   P_next;
    time_save(n) = time;
    
    % Save the synthetics at stations
    P_save = ???
    
    % Plot solution every 50 timesteps
    if (mod(n,50)==0)
        figure(1), clf
        pcolor(x2d,z2d,P); shading interp, colorbar
        hold on
        plot(sta_x,sta_z,'r^');
        xlabel('x [m]')
        ylabel('z [m]')
        zlabel('Pressure [Pa]')
        title(['Wave propagation after T = ',num2str(time),' sec'])
        axis equal, axis tight
        drawnow
    end
end
figure;
% Plot the station records
plot(time_save,P_save);
xlabel('Time [sec]');
ylabel('Amp');
legend(?????);