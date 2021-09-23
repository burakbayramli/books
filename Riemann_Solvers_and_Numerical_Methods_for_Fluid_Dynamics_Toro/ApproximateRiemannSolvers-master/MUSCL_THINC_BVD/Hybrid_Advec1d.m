%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%         Solving 1-D wave equation Hybrid numerical schemes, namely
%
%             Tangent Hyperbola for INterface Capturing and
%             Monotonic Upwind Scheme for Conservation Laws 
%                          (MUSCL-THINC-BVD) scheme
%
%                                  &
%
%               Tangent Hyperbola for INterface Capturing and
%                    Weighted Essentially Non-Oscilaroty
%                          (WENO5-THINC-BVD) scheme
%
%
%                 du/dt + df/dx = S, for x \in [a,b]
%                  where f = f(u): linear/nonlinear
%                     and S = s(u): source term
%
%
%             coded by Manuel Diaz, manuel.ade'at'gmail.com 
%            Institute of Applied Mechanics, NHRI, 2018.06.20
%                               
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ref: 
% [1] Deng, Xi, Bin Xie, and Feng Xiao. "Some practical versions of
%     boundary variation diminishing (BVD) algorithm." arXiv preprint
%     arXiv:1708.01148 (2017).  
% [2] Deng, Xi, et al. "Limiter-free discontinuity-capturing scheme for 
%     compressible gas dynamics with reactive fronts." C & F (2018).
% [3] Deng, Xi, et al. "High fidelity discontinuity-resolving
%     reconstruction for compressible multiphase flows with moving 
%     interfaces." Journal of Computational Physics (2018). 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Notes: 
% -------
% The present implementation serves for the purpose of comparions and a
% summary of the main hybrid algorithms reported in [1-3]. In this
% snippets, I prioritize readability rather than code performance. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clear; %close all; clc;

%% Parameters
   nx = 0200;	% number of cells
  CFL = 0.50;	% Courant Number
 tEnd = 2.00;   % End time
limit = 'MM';   % MC, MM, VA. (only for Methd = {5,6})
Methd = 5;      % 1:WENO5, 2:WENO5-THINC-BVDv2, 3:WENO5-THINC-BVDv4, 
                % 4:THINC, 5:MUSCL-THINC-BVDv2, 6:MUSCL-THINC-BVDv4.             
% NOTE:
%  XMETHOD-THINC-BVDv1 (algorithm 1) is similar to algorithm 4 and
%  XMETHOD-THINC-BVDv3 (algorithm 3) is the multidimensional formulation.
%   WENO5-THINC-BVDv2  (algorithm 2) seems to be the most stable formulation.

fluxfun='buckley'; % select flux function
% Define our Flux function
switch fluxfun
    case 'linear'   % Scalar Advection, CFL_max: 0.65
        c=1; flux = @(w) c*w; 
        dflux = @(w) c*ones(size(w));
        ICcase=1; tEnd=2.0;
    case 'burgers' % Burgers, CFL_max: 0.40  
        flux = @(w) w.^2/2; 
        dflux = @(w) w;
        ICcase=2; tEnd=2.0; CFL=0.4; IC=4;
    case 'buckley' % Buckley-Leverett, CFL_max: 0.20 & tEnd: 0.40
        flux = @(w) 4*w.^2./(4*w.^2+(1-w).^2);
        dflux = @(w) 8*w.*(1-w)./(5*w.^2-2*w+1).^2;
        ICcase=2; tEnd=0.4; CFL=0.2; IC=9;
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
%ICcase=2; % overide for % {1}Testing, {2}Costum ICs
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

switch Methd
    case 1 % 5th-order WENO 
        Method = 'WENO5';
        FlxSplt = true;
        while t < tEnd
            % Update/correct time step
            dt=CFL*dx/max(abs(u)); if t+dt>tEnd, dt=tEnd-t; end

            % Update time and iteration counter
            t=t+dt; it=it+1;

            % RK Initial step
            uo = u;

            % 1st stage
            if ~FlxSplt, L=WENO5_AdvecRes1d(u,flux,dflux,S,dx); end
            if FlxSplt, L=WENO5_AdvecRes1d_FluxSplitting(u,flux,dflux,S,dx); end
            u = uo-dt*L;

            % 2nd Stage
            if ~FlxSplt, L=WENO5_AdvecRes1d(u,flux,dflux,S,dx); end
            if FlxSplt, L=WENO5_AdvecRes1d_FluxSplitting(u,flux,dflux,S,dx); end
            u = 0.75*uo+0.25*(u-dt*L);

            % 3rd stage
            if ~FlxSplt, L=WENO5_AdvecRes1d(u,flux,dflux,S,dx); end
            if FlxSplt, L=WENO5_AdvecRes1d_FluxSplitting(u,flux,dflux,S,dx); end
            u = (uo+2*(u-dt*L))/3;

            % Plot solution
            if rem(it,10) == 0
                plot(x,u0,'-x',x,u,'.'); axis(plotrange); shg; drawnow;
            end
        end
    case 2 % WENO5-THINC-BVD Algorithm 2
        Method = 'WENO5-THINC-BVD v2';
        while t < tEnd
            % Update/correct time step
            dt=CFL*dx/max(abs(u)); if t+dt>tEnd, dt=tEnd-t; end

            % Update time and iteration counter
            t=t+dt; it=it+1;

            % RK Initial step
            uo = u;

            % 1st stage
            L = Hybrid_AdvecRes1d_Algorithm2(u,flux,dflux,S,dx);
            u = uo-dt*L;

            % 2nd Stage
            L = Hybrid_AdvecRes1d_Algorithm2(u,flux,dflux,S,dx);
            u = 0.75*uo+0.25*(u-dt*L);

            % 3rd stage
            L = Hybrid_AdvecRes1d_Algorithm2(u,flux,dflux,S,dx);
            u = (uo+2*(u-dt*L))/3;

            % Plot solution
            if rem(it,10) == 0
                plot(x,u0,'-x',x,u,'.'); axis(plotrange); shg; drawnow;
            end
        end
    case 3 % WENO5-THINC-BVD Algorithm 4
        Method = 'WENO5-THINC-BVD v4';
        while t < tEnd
            % Update/correct time step
            dt=CFL*dx/max(abs(u)); if t+dt>tEnd, dt=tEnd-t; end

            % Update time and iteration counter
            t=t+dt; it=it+1;

            % RK Initial step
            uo = u;

            % 1st stage
            L = Hybrid_AdvecRes1d_Algorithm4(u,flux,dflux,S,dx);
            u = uo-dt*L;

            % 2nd Stage
            L = Hybrid_AdvecRes1d_Algorithm4(u,flux,dflux,S,dx);
            u = 0.75*uo+0.25*(u-dt*L);

            % 3rd stage
            L = Hybrid_AdvecRes1d_Algorithm4(u,flux,dflux,S,dx);
            u = (uo+2*(u-dt*L))/3;

            % Plot solution
            if rem(it,10) == 0
                plot(x,u0,'-x',x,u,'.'); axis(plotrange); shg; drawnow;
            end
        end
    case 4 % Full THINC-BVD formulation
        Method = 'THINC-BVD';
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
    case 5 % MUSCL-THINC-BVD Algorithm 2
        Method = 'MUSCL-THINC-BVD v2';
        %limiter= 'MM';
        while t < tEnd
            % Update/correct time step
            dt=CFL*dx/max(abs(u)); if t+dt>tEnd, dt=tEnd-t; end

            % Update time and iteration counter
            t=t+dt; it=it+1;

            % RK Initial step
            uo = u;

            % 1st stage
            L = Hybrid2_AdvecRes1d_Algorithm2(u,flux,dflux,S,dx,limit);
            u = uo-dt*L;

            % 2nd Stage
            L = Hybrid2_AdvecRes1d_Algorithm2(u,flux,dflux,S,dx,limit);
            u = 0.75*uo+0.25*(u-dt*L);

            % 3rd stage
            L = Hybrid2_AdvecRes1d_Algorithm2(u,flux,dflux,S,dx,limit);
            u = (uo+2*(u-dt*L))/3;

            % Plot solution
            if rem(it,10) == 0
                plot(x,u0,'-x',x,u,'.'); axis(plotrange); shg; drawnow;
            end
        end
    case 6 % MUSCL-THINC-BVD Algorithm 4
        Method = 'MUSCL-THINC-BVD v4';
        %limiter= 'MM';
        while t < tEnd
            % Update/correct time step
            dt=CFL*dx/max(abs(u)); if t+dt>tEnd, dt=tEnd-t; end

            % Update time and iteration counter
            t=t+dt; it=it+1;

            % RK Initial step
            uo = u;

            % 1st stage
            L = Hybrid2_AdvecRes1d_Algorithm4(u,flux,dflux,S,dx,limit);
            u = uo-dt*L;

            % 2nd Stage
            L = Hybrid2_AdvecRes1d_Algorithm4(u,flux,dflux,S,dx,limit);
            u = 0.75*uo+0.25*(u-dt*L);

            % 3rd stage
            L = Hybrid2_AdvecRes1d_Algorithm4(u,flux,dflux,S,dx,limit);
            u = (uo+2*(u-dt*L))/3;

            % Plot solution
            if rem(it,10) == 0
                plot(x,u0,'-x',x,u,'.'); axis(plotrange); shg; drawnow;
            end
        end
    otherwise
        error('Case not in list');
end

%% Final Plot
plot(x,u0,'-x',x,u,'.'); axis(plotrange);
title([Method,', cell averages plot'],'interpreter','latex','FontSize',18);
xlabel('$\it{x}$','interpreter','latex','FontSize',14);
ylabel({'$\it{u(x)}$'},'interpreter','latex','FontSize',14);