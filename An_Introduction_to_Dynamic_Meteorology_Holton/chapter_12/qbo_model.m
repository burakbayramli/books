% MATLAB file:  qbo_model.m
% 1-d analogue for the tropical QBO.
% Based on Plumb (1977) J. Atmos. Sci. article.
% Uses 3rd order Adam-Bashforth time defferencing.
% Forcing is specified in terms of lower boundary momentum flux
% by two gravity waves of equal and opposite flux and phase speed.
% Equations are nondimensionalized for convenience.
% ub field is saved every 2 time units.
% Gridpoint labelling starts with j=1 at z = dz= .05.  
clear all
close all
% Define  constants and parameters
J = 80;                             % number of vertical gridpoints
ztop = 4;                           % top boundary level 
Jl = J -1;
Am = input('specify amplitude of boundary forcing in range 0.04-0.4: ');
k1 = 1; k2 = 1;                     % zonal wavenumbers for the two waves
c1 = 1; c2 = -1;                    % phase speeds for the two waves
% setup grid and define basic state  
time_end = input('specify time units for integration (100 minimum) ');
dz = ztop/J;
ubtime = zeros(J+1, time_end/2);    % time series for mean wind
G1 = zeros(J,time_end);             % time series for wave 1 flux
G2 = zeros(J,time_end);             % time series for wave 2 flux
count = 0;                          % time interval counter
z = linspace(dz, ztop, J);          % vertical prediction levels
zplot = [0 z];
dt = .01   ;                        % time step  
save_t = fix(2/dt);                 % time step interval for saving data (integer)
nsave = 0;                          % counter for saving data
day = linspace(1,time_end,time_end/2);
dt12 = dt/12;
t = 0;                              % initial time
alph = 1;                           % GW damping rate
lambda = 0.02;                      % mean flow diffusion
% 
dz2 = dz^2;
% note that z=0 and z=ztop correspond to j = 0 and j = J
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ub0 = 0;                            % mean wind at z = 0
ub = 0.2*sin(pi*z/4);               % initial mean wind profile
F1n = zeros(size(z));               % vector for wave 1 forcing at time n
F2n = F1n;                          % vector for wave 2 forcing at time n
Force=F1n;                          % vector for total forcing at time n
Force1 = Force; Force2 = Force;     % total forcing at time n-1 and n-2
%
% time integration by Adams-Bashforth 3rd order differencing
ntime = time_end/dt;                % total number of time steps
% Begin time stepping
for n = 1:ntime
    
    %%%%%%    time stepping for mean wind equation
    % first compute vertical dependence of momentum flux divergence
    G1n = alph./(k1*(ub-c1).^2); 
    G2n = alph./(k2*(ub-c2).^2);
    G10 = alph./(k1*(ub0-c1).^2);
    G20 = alph./(k2*(ub0-c2).^2);
    % compute the eastward and westward wave forcing
    for j = 1:J
        F1n(j) = Am*G1n(j).*exp(-(0.5*(G10+G1n(j))+sum(G1n(1:j-1)))*dz);
        F2n(j) = -Am*G2n(j).*exp(-(0.5*(G20+G2n(j))+sum(G2n(1:j-1)))*dz);
    end
    % now add wave plus mean flow diffusion terms to  get total forcing
    Force(2:Jl) = F1n(2:Jl)+F2n(2:Jl)...
        +lambda/dz2*(ub(3:J)-2*ub(2:J-1)+ub(1:J-2));
    Force(1) = F1n(1)+F2n(1) +lambda/dz2*(ub(2)-2*ub(1)+ub0);
    % advance ubar by one timestep
    ubm(1:Jl) = ub(1:Jl)+ ...
        dt12*(23*Force(1:Jl)' -16*Force1(1:Jl)' +5*Force2(1:Jl)')';
    ubm(J) = ubm(Jl);
    ub = ubm;                       %updates the ub field
    Force2 = Force1;
    Force1 = Force;
    %%%%%%  end of mean wind time step
    t = t + dt;
    count = count +1;
    if count == save_t; 
        nsave = nsave+1;
        ubplot = [ub0 ub];
        count = 0;
        str1 = ['t = ' num2str(t)];
        ubtime(:,nsave) = ubplot';
        zplot = [0 z];
        figure(1)
        plot(ubplot,zplot,'k');
        axis([-1 1 0 4]);
        xlabel ('zonal wind (nondimensional)')
        ylabel  ('height (nondimensional)')
        title('mean zonal wind')
        text(-.8, 3.5, str1)
    end
end
End = fix(time_end);
figure(2)
plot(day,ubtime(10,:),day,ubtime(40,:),'--k')
xlabel('time (nondimensional) '), ylabel('velocity (nondimensional)  ')
axis([0 End -1 1])
title('mean zonal wind (nondimensional) at .5 (solid) and 2 (dashed)')
[daygrd,yy] = meshgrid(day,zplot);
figure(3)
pcolor(daygrd(:,:),yy(:,:),ubtime(:,:))
caxis([-1 1])
xlabel('time'), ylabel('height (nondimensional) ')
title('mean zonal wind (nondimensional)')
shading interp
hold on
contour(daygrd(:,:),yy(:,:),ubtime(:,:),[-.8 -.6 -.4 -.2 0 .2 .4 .6 .8],'k')
colorbar('v')
