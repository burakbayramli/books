
clear
close all

% Preparation for the DG wave equation
%
% Advection equation
%
% d/dt v + c d/dx v = g 

% plot pars
lw=1;
fs=12;

% Setup
nx=1250;
c=2500; % acoustic velocity in m/s
xmax=10000;  % in m 
eps=.5; % CFL
tmax=3;  % simulation time in s
isnap=50;
is=floor(nx-10); % Source location
sig=100;
x0=1000;

% Space
x=linspace(0,xmax,nx);
dx=min(diff(x));

% use wave based CFL criterion
dt=eps*dx/c; % calculate tim step from stability criterion

% Simulation time
nt=floor(tmax/dt);



%Spatial initial condition
sx=exp(-1./sig^2*(x-x0).^2);

figure
plot(x,sx)
axis([0 3*x0 -Inf Inf])

% Initialize fields
vnew=x*0;
v=vnew;
dv=v;


% Solution with finite differences

% initial condition
vnew=sx;

figure
for i=1:nt;
    
    v=vnew;
    for j=2:nx-1,
        
        % forward (upwind) (c < 0)
        dv(j)=(v(j)-v(j-1))/dx;
        % centered
        %dv(j)=(v(j+1)-v(j-1))/(2*dx);
    end
    % time extrapolation
    vnew=v-dt*c*dv;
    
    % Plotting
    if rem(i,isnap)==0,
        plot(x,vnew,'k-','LineWidth',lw), hold on
        xlabel(' x (m) ')
        ylabel(' Amplitude ')
        title(' Upwind finite-difference scheme ')
        set(gca,'Fontsize',fs,'FontName','Times New Roman')
        drawnow 
    end
end
    
        