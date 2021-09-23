%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%              Solving 1-D wave equation with 2nd-order
%        Tangent Hyperbola for INterface Capturing (THINC) methods
%
%                 du/dt + df/dx = S, for x \in [a,b]
%                  where f = f(u): linear/nonlinear
%                     and S = s(u): source term
%
%             coded by Manuel Diaz, manuel.ade'at'gmail.com 
%            Institute of Applied Mechanics, NHRI, 2018.06.20
%                               
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Refs:
% [1] Deng, Xi, Bin Xie, and Feng Xiao. "Some practical versions of
%     boundary variation diminishing (BVD) algorithm." arXiv preprint
%     arXiv:1708.01148 (2017).  
% [2] Deng, Xi, et al. "Limiter-free discontinuity-capturing scheme for 
%     compressible gas dynamics with reactive fronts." C & F (2018). 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NOTE:
% Thanks to Kenny Lozes for pointing out that the THINC reconstruction, as
% described in [2], cannot upwind by itself. It appears that this is not
% well detailed by the authors. Therefore, such condition must be provided
% for the scenario where (q(i+1)-q(i))*(q(i)-q(i-1)) < 0, takes place. 
% See details in the code.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clear; %close all; clc;

%% Parameters
   nx = 0200;	% number of cells
  CFL = 0.50;	% Courant Number
 tEnd = 0.50;   % End time

fluxfun='linear'; % select flux function
% Define our Flux function
switch fluxfun
    case 'linear'   % Scalar Advection, CFL_max: 0.65
        c=1; flux = @(w) c*w; 
        dflux = @(w) c*ones(size(w));
        ICcase=1; tEnd=2.0;
    case 'burgers' % Burgers, CFL_max: 0.40  
        flux = @(w) w.^2/2; 
        dflux = @(w) w;
        ICcase=2; CFL=0.4; IC=4; tEnd=1;
    case 'buckley' % Buckley-Leverett, CFL_max: 0.20 & tEnd: 0.40
        flux = @(w) 4*w.^2./(4*w.^2+(1-w).^2);
        dflux = @(w) 8*w.*(1-w)./(5*w.^2-2*w+1).^2;
        ICcase=2; CFL=0.2; IC=9; tEnd=0.4;
end

sourcefun='dont'; % add source term
% Source term
switch sourcefun
    case 'add'
        S = @(w) 0.1*w.^2;
    case 'dont'
        S = @(w) zeros(size(w));
end

% Build discrete domain
a=-1; b=1; dx=(b-a)/nx; x=a+dx/2:dx:b; 

% Build IC
%ICcase=2;  % overide for: {1}Testing, {2}Costum ICs
switch ICcase
    case 1 % Testing IC
        u0=TestingIC(x);  % Jiang and Shu IC
    case 2 % Guassian IC
        u0=CommonIC(x,IC); % cases 1-10 <- check them out!
    otherwise
        error('IC file not listed');
end

% Plot range
dl=0.1; plotrange=[a,b,min(u0)-dl,max(u0)+dl];

%% Solver Loop

% load initial conditions
t=0; it=0; u=u0;

% Full THINC-BVD formulation
while t < tEnd
    % Update/correct time step
    dt=CFL*dx/max(abs(u)); if t+dt>tEnd, dt=tEnd-t; end

    % Update time and iteration counter
    t=t+dt; it=it+1;

    % RK Initial step
    uo = u;

    % 1st stage
    L = THINC_AdvecRes1d(u,flux,dflux,S,dx);
    u = uo-dt*L;

    % 2nd Stage
    L = THINC_AdvecRes1d(u,flux,dflux,S,dx);
    u = 0.75*uo+0.25*(u-dt*L);

    % 3rd stage
    L = THINC_AdvecRes1d(u,flux,dflux,S,dx);
    u = (uo+2*(u-dt*L))/3;

    % Plot solution
    if rem(it,10) == 0
        plot(x,u0,'-x',x,u,'.'); axis(plotrange); shg; drawnow;
    end
end

%% Final Plot
plot(x,u0,'-x',x,u,'.'); axis(plotrange);
title('Adaptative THINC-BVD, cell averages plot','interpreter','latex','FontSize',18);
xlabel('$\it{x}$','interpreter','latex','FontSize',14);
ylabel({'$\it{u(x)}$'},'interpreter','latex','FontSize',14);