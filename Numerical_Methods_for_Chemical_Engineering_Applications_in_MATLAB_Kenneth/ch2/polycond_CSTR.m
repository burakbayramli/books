% polycond_CSTR.m
% This program plots the number average chain
% length and polydispersity of a polymer
% produced by polycondensation (with balanced
% end group concentrations) in a CSTR.
% Input values:
% zeta = dimensionless ratio of monomer and condensate
%        molecular weights
% K_eq = equilibrium constant of reaction
% omega_in = dimensionless concentration of water in inlet
% mu_0_in = inlet value of zeroth moment of dimensionless
%        chain length distribution
% mu_2_in = inlet value of second moment of
%        dimensionless chain length distribution
% K.J. Beers. 2/20/2005.  ver. 2/21/2005.B.

function [iflag_main,Grid] = polycond_CSTR();
iflag_main = 0;

% ask user to input values of fixed system parameters
SysFixed = get_SysFixed();

% ask user to input simulation parameters
Sim = get_Sim();

% set values to scan of two variable parameters,
% gamma (the relative importance of mass transfer
% of water out of the reactor) and Da (the
% Damkoehler number).
Grid.gamma = logspace(Sim.gamma_log10_lo,...
    Sim.gamma_log10_hi,Sim.N);
Grid.Da = logspace(Sim.Da_log10_lo, ...
    Sim.Da_log10_hi,Sim.N);
% allocate memory to store results
Grid.xn = zeros(Sim.N,Sim.N);
Grid.Z = zeros(Sim.N,Sim.N);
Grid.omega = zeros(Sim.N,Sim.N);
Grid.phi = zeros(Sim.N,Sim.N);

% for each combination of the two parameters,
% compute the steady-state properties of the
% polymer produced by the CSTR.
Options = optimset('Largescale','off', ...
    'Display','off','TolFun', Sim.TolFun);

for ig = 1:Sim.N
    disp(' ');
    disp(['gamma iteration # ', int2str(ig)]);
    for iDa = 1:Sim.N
        disp(['   Da iteration # ', int2str(iDa)]);
        % complete structure of system
        % parameters with current values of
        % variable parameters
        Sys = SysFixed;
        Sys.gamma = Grid.gamma(ig);
        Sys.Da = Grid.Da(iDa);
        % get initial guess of parameters
        % for highest Da value, use values
        % of moments for inlets.  Otherwise,
        % use results of last computation.
        if(iDa == 1)
            x_0 = [Sys.mu_0_in; Sys.mu_2_in; ...
                Sys.omega_in];
            % if any of these are less than
            % trace, reset initial guess to
            % 2*trace
            k_too_lo = find(x_0 < Sim.trace);
            if(k_too_lo)
                x_0(k_too_lo) = 2*Sim.trace;
            end
        else
            x_0 = x;
        end
        % before starting Newton's method, try
        % running dynamic simulation to relax
        % the system.
        x_0 = relax_dyn(x_0, Sys, Sim);
        % calculate the zeroth and second moment
        % of the chain length distribution at
        % steady state
        [x,f,exitflag] = ...
            fsolve(@calc_f, x_0, Options, Sys, Sim);
        f_norm = norm(f,inf);
        if(exitflag ~= 1)
            phrase = ['FSOLVE(', int2str(ig), ...
                ',', int2str(iDa), ...
                '): exitflag = ', ...
                int2str(exitflag), ...
                ', ||f|| = ', num2str(f_norm)];
            if(Sim.isterminal)
                error(phrase);
            else
                disp(phrase);
            end
        else
            disp(['      ', ...
                '||f|| = ', num2str(f_norm)]);
        end

        % extract values from state vector
        mu_0 = x(1);  mu_2 = x(2);  omega = x(3);
        % calculate values of auxilliary variables
        [phi,mu_3] = calc_aux_var(mu_0,mu_2,omega,Sys);
        % compute number and weight average chain
        % lengths and the polydispersity
        xn = 1/mu_0;  xw = mu_2;  Z = xw/xn;
        % store results
        Grid.xn(iDa,ig) = xn;
        Grid.Z(iDa,ig) = Z;
        Grid.omega(iDa,ig) = omega;
        Grid.phi(iDa,ig) = phi;
    end
end

% plot the results
plot_results(Grid);

iflag_main = 1;
return;


% ====================
% This routine asks the user to input the values
% of the fixed system parameters.
function SysFixed = get_SysFixed();

disp('Input fixed system parameters ...');

disp(' ');
disp('zeta is the dimensionless ratio of condensate');
disp('to monomer molecular weights (0.2).');
SysFixed.zeta = input('Enter zeta : ');

disp(' ');
disp('K_eq is the equilibrium constant (100) : ');
SysFixed.K_eq = input('Enter K_eq : ');

disp(' ');
disp('omega_in is the dimensionless concentration of');
disp('condensate in the inlet stream (0).');
SysFixed.omega_in = input('Enter omega_in : ');

disp(' ');
disp('mu_0_in is the dimensionless zeroth moment of the');
disp('chain length distribution.  It is one for momomer feed.');
SysFixed.mu_0_in = input('Enter mu_0_in : ');

disp(' ');
disp('mu_2_in is the dimensionless second moment of the');
disp('chain length distribution.  It is one for monomer feed.');
SysFixed.mu_2_in = input('Enter mu_2_in : ');

return;


% ====================
% This routine asks the user to input the
% simulation parameters.
function Sim = get_Sim();

disp(' ');
disp('Next, enter simulation parameters ...');
disp(' ');

Sim.TolFun = input(...
    'Enter TolFun (1e-12) : ');
Sim.trace = input(...
    'Enter trace conc. level (1e-15) : ');
Sim.isterminal = input(...
    'Stop calculations when exitflag~=1 (0=n, 1=y) : ');
Sim.f_norm_relax = input(...
    'Enter ||f|| at which to stop dynamic relaxation : ');
Sim.gamma_log10_lo = input(...
    'Enter low log10(gamma) (-4) : ');
Sim.gamma_log10_hi = input(...
    'Enter high log10(gamma) (2) : ');
Sim.Da_log10_lo = input(...
    'Enter low log10(Da) (-2) : ');
Sim.Da_log10_hi = input(...
    'Enter high log10(Da) (3) : ');
Sim.N = input(...
    'Enter # of grid points in 1-D (10) : ');

return;


% ====================
% This routine performs a dynamics simulation of the
% system until the function norm drops below a
% specified value.
function x = relax_dyn(x_0, Sys, Sim);

% simulate in dimensionless time periods of 1
% until the function norm drops below the
% specified max. allowable value.
for k=1:100
    [t_traj,x_traj] = ...
        ode23s(@calc_xdot, [0:0.5:1], x_0, [], ...
            Sys, Sim);
    x = x_traj(3,:)';
    f = calc_f(x, Sys, Sim);
    f_norm = norm(f,inf);
    if(f_norm <= Sim.f_norm_relax)
        disp('      relaxed dynamics');
        break;
    end
    x_0 = x;
    k_too_lo = find(x_0 < Sim.trace);
    if(k_too_lo)
        x_0(k_too_lo) = 2*Sim.trace;
    end
end
    
return;


% ====================
% This returns the time derivative vector
% for the dynamic relaxation simulation.
function xdot = calc_xdot(t, x, Sys, Sim);

xdot = calc_f(x, Sys, Sim);

return;


% ====================
% This routine results the function vector for
% the polycondensation CSTR.
function f = calc_f (x, Sys, Sim);

f = zeros(3,1);

% allocate moments
mu_0 = x(1);  mu_2 = x(2);  omega = x(3);
% ensure positivity
if(mu_0 < Sim.trace)
    f(1) = Sim.trace - mu_0;  % to increase value
    mu_0 = Sim.trace;
end
if(mu_2 < Sim.trace)
    f(2) = Sim.trace - mu_2;  % to increase value
    mu_2 = Sim.trace;
end
if(omega < Sim.trace)
    f(3) = Sim.trace - omega;  % to increase value
    omega = Sim.trace;
end

% calculate values of auxilliary variables
[phi,mu_3] = calc_aux_var(mu_0,mu_2,omega,Sys);

% compute function values, if not done above
% when values are below trace
% mu_0 balance
if(~f(1))
    f(1) = Sys.mu_0_in - phi*mu_0 - Sys.Da*(mu_0^2) ...
        + Sys.Da/Sys.K_eq*omega*(1 - mu_0);
end
% mu_2 balance
if(~f(2))
    f(2) = Sys.mu_2_in - phi*mu_2 + 2*Sys.Da ...
        + Sys.Da/Sys.K_eq*omega*(1/3 - mu_3);
end
% omega balance
if(~f(3))
    f(3) = Sys.omega_in - phi*omega ...
        - Sys.gamma*omega ...
        + Sys.Da*(mu_0^2) ...
        - Sys.Da/Sys.K_eq*omega*(1 - mu_0);
end

return;


% ==========
% This routine computes the values of auxilliary
% variables.
function [phi, mu_3] = ...
    calc_aux_var(mu_0, mu_2, omega, Sys);

% relative outlet flow rate to inlet flow rate
phi = 1 - Sys.gamma*Sys.zeta*omega;

% compute third moment from closure approximation
mu_3 = mu_2*(2*mu_2*mu_0 - 1)/mu_0;

return;


% ====================
% This routine plots the results of the
% calculation to a figure.
function iflag = plot_results(Grid);
iflag = 0;

x_vals = log10(Grid.gamma);
y_vals = log10(Grid.Da);

figure;
subplot(2,2,1);  surf(x_vals,y_vals,Grid.xn);
xlabel('log_{10} \gamma');  ylabel('log_{10} Da');
zlabel('x_n');  axis tight;

subplot(2,2,2);  surf(x_vals,y_vals,Grid.Z);
xlabel('log_{10} \gamma');  ylabel('log_{10} Da');
zlabel('Z');  axis tight;

subplot(2,2,3);  surf(x_vals,y_vals,Grid.omega);
xlabel('log_{10} \gamma');  ylabel('log_{10} Da');
zlabel('\omega');  axis tight;

subplot(2,2,4);  surf(x_vals,y_vals,Grid.phi);
xlabel('log_{10} \gamma');  ylabel('log_{10} Da');
zlabel('\phi');  axis tight;

iflag = 1;
return;
