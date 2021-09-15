% tubular_reactor_2rxn_dyn_sim.m
% This MATLAB program computes the concentration profiles within a
% tubular rector for the system of two elementary reactions
%   A + B --> C
%   C + B --> D
% The model includes both convection and dispersion, and uses
% upwind finite differences to discretize the system at a grid
% of N uniformly-spaced points within the reactor. The program
% is modified from the steady-state program to simulate the
% dynamics of reactor start-up.
% K. Beers. MIT ChE. 10/7/03
% ver. 11/17/2004 (contains all necessary files to perform simulation)
function iflag_main =  tubular_reactor_2rxn_dyn_sim();
iflag_main = 0;

% set problem specifications
Param.L = 10;  % reactor length
Param.k = [1; 1];  % rate constants for rxn. 1 and 2
Param.c_in = [1; 1; 0; 0];  % inlet conc. of A,B,C,D
Param.v = 1;  % axial velocity in reactor
Param.D = 1e-4;  % dispersion coefficient
Param.N_f = 4;  % total number of fields
% Next, give name of user-supplied routine that computes the reaction rates
% and the derivatives of the reaction rates with respect to each
% concentration.
Param.scheme_rxn = 'scheme_2rxn';

% set problem dimensions
Grid.N = 100;  % 100 grid points
Grid.dz = Param.L/(Grid.N + 1);  % uniform grid spacing
Grid.z = linspace(Grid.dz,Param.L-Grid.dz,Grid.N)';  % set uniform grid
Grid.N_var = Grid.N*Param.N_f;  % total # of unknowns
Grid.Pe_loc = Param.v*Grid.dz/Param.D;  % local Peclet number of grid

% as initial values, use uniform profile of [A] = 2, [B]=[C]=[D]=0
x0 = zeros(Grid.N_var,1);  % allocate memory
for k=1:Grid.N  % for each grid point
    n = tubular_reactor_get_label(1,k,Param.N_f);  % position of [A]
    x0(n) = 2;
end

% compute mean residence time, and generate vector of times at which to
% compute concentration profile
Param.t_res = Param.L/Param.v;  % reactor residence time
sim_period = Param.t_res / 5;
t_sim_vect = [0: sim_period : 2*Param.t_res];

% call ode15s to perform dynamic simulation
Options_ODE = odeset('BDF','on', ...
    'Jacobian','tubular_reactor_2rxn_dyn_calc_Jac');
[t_traj,x_traj] = ode15s(@tubular_reactor_2rxn_dyn_calc_f, ...
    t_sim_vect,x0,Options_ODE,Param,Grid);

% plot results
figure;
z_plot = [0; Grid.z; Param.L];  % extended to include inlet, outlet positions
num_frames = length(t_traj);  % number of frames in trajectory
% extract and plot concentration profile of A (use BC to generate
% values at the boundaries)
subplot(2,2,1);
for itime=1:num_frames   % for each time in trajectory file
    field_val = extract_field(1,x_traj(itime,:)',Grid,Param);
    if(itime==1)
        plot(z_plot,field_val);  hold on;
    elseif(itime==num_frames)
        plot(z_plot,field_val);
    else
        plot(z_plot,field_val,'-.');
    end
end
xlabel('z');  ylabel('[A]');
title('Tubular reactor start-up dynamics');
% concentration profiles of B
subplot(2,2,2);
for itime=1:size(t_traj,1)
    field_val = extract_field(2,x_traj(itime,:)',Grid,Param);
    if(itime==1)
        plot(z_plot,field_val);  hold on;
    elseif(itime==num_frames)
        plot(z_plot,field_val);
    else
        plot(z_plot,field_val,'-.');
    end
end
xlabel('z');  ylabel('[B]');
% concentration profiles of C
subplot(2,2,3);
for itime=1:size(t_traj,1)
    field_val = extract_field(3,x_traj(itime,:)',Grid,Param);
    if(itime==1)
        plot(z_plot,field_val);  hold on;
    elseif(itime==num_frames)
        plot(z_plot,field_val);
    else
        plot(z_plot,field_val,'-.');
    end
end
xlabel('z');  ylabel('[C]');
% concentration profiles of D
subplot(2,2,4);
for itime=1:size(t_traj,1)
    field_val = extract_field(4,x_traj(itime,:)',Grid,Param);
    if(itime==1)
        plot(z_plot,field_val);  hold on;
    elseif(itime==num_frames)
        plot(z_plot,field_val);
    else
        plot(z_plot,field_val,'-.');
    end
end
xlabel('z');  ylabel('[D]');

iflag_main = 1;
return;



% ================================================================
% tubular_reactor_2rxn_dyn_calc_f.m
% This routine returns the vector of time derivatives. It merely
% serves as a wrapper and invokes the steady-state function to do the
% actual calculation.
function f = tubular_reactor_2rxn_dyn_calc_f(t,x,Grid,Param);

f = tubular_reactor_2rxn_SS_calc_f(x,Grid,Param);

return;



% ---------------------------------------------------------------------
% tubular_reactor_2rxn_dyn_calc_Jac.m
% This routine returns the Jacobian matrix. It merely serves as a
% wrapper and invokes the steady-state function to do the actual
% calculation. The ODE solvers split the function and Jacobian
% evaluations because they do not need to calculate new Jacobians
% at every time step.
function Jac = tubular_reactor_2rxn_dyn_calc_Jac(t,x,Grid,Param);

[f,Jac] = tubular_reactor_2rxn_SS_calc_f(x,Grid,Param);

return;




%----------------------------------------------------------------------
% This routine computes the function vector and the Jacobian of the
% tubular reactor model. It calls a user-supplied routine that
% computes the rates of each reaction, the derivatives of the
% reaction rates with respect to each component, and returns the
% stoichiometric coefficients. From this information, the routine
% computes the function vector and the Jacobian.
function [f,Jac] = tubular_reactor_2rxn_SS_calc_f(x,Param,Grid);

% allocate space in memory
f = zeros(Grid.N_var,1);
nnz_Jac_max = Grid.N_var * (Param.N_f + 2);
Jac = spalloc(Grid.N_var,Grid.N_var,nnz_Jac_max);


% set common coefficients for lower and upper grid points for discretizing
% the transport terms
A_lo = Param.v/Grid.dz + Param.D/(Grid.dz^2);
A_mid = -Param.v/Grid.dz - 2*Param.D/(Grid.dz^2);
A_hi = Param.D/(Grid.dz^2);

% for each point
for k=1:Grid.N

    % extract concentrations of each field variable at grid point
    % conc_loc = (  [A], [B], [C], [D] )
    conc_loc = zeros(Param.N_f,1);
    for ifield=1:Param.N_f
        n = tubular_reactor_get_label(ifield,k,Param.N_f);
        conc_loc(ifield) = x(n);
    end
    
    % compute the reaction rates and the partial derivatives of each
    % rate with respect to each local concentration. This is done in
    % the routine supplied by the user whose name is stored in the
    % character string Param.rxn_scheme.
    [num_rxn,rxn_rate,stoich_coeff,rxn_rate_deriv] = ...
        feval(Param.scheme_rxn,conc_loc,Param);

    % compute local source term values of each species
    source = zeros(Param.N_f,1);
    for ifield=1:Param.N_f
        for irxn=1:num_rxn
            source(ifield) = source(ifield) + rxn_rate(irxn)*stoich_coeff(ifield,irxn);
        end
    end  

    % compute source term Jacobianm matrix for which element (i,j)
    % is the partial derivative of the source term for field # i
    % with respect to the local value of field # j
    source_Jac = zeros(Param.N_f,Param.N_f);
    for ifield=1:Param.N_f
        for irxn=1:num_rxn
            for jfield=1:Param.N_f
                source_Jac(ifield,jfield) = source_Jac(ifield,jfield) + ...
                    rxn_rate_deriv(jfield,irxn)*stoich_coeff(ifield,irxn);
            end
        end
    end
    
    % Now, initialize function vector and Jacobian elements for this grid
    % point to contain the source term and the source term Jacobian
    for ifield=1:Param.N_f
        n = tubular_reactor_get_label(ifield,k,Param.N_f);
        f(n) = source(ifield);
        for jfield=1:Param.N_f  % for all fields
            m = tubular_reactor_get_label(jfield,k,Param.N_f);
            Jac(n,m) = source_Jac(ifield,jfield);
        end
    end
    
    % Now, get transport contribution to function and Jacobian values
    for ifield=1:Param.N_f
        n = tubular_reactor_get_label(ifield,k,Param.N_f);
        
        % dependence of transport on field value itself
        f(n) = f(n) + A_mid*x(n);
        Jac(n,n) = Jac(n,n) + A_mid;
        
        % dependence on value to the left
        if(k > 1)  % if not the left-most point
            m = tubular_reactor_get_label(ifield,k-1,Param.N_f); % value of same field at left point
            f(n) = f(n) + A_lo*x(m);
            Jac(n,m) = Jac(n,m) + A_lo;
        else  % use Danckwerts' BC to estimate conc. at inlet
            c_lo = (Grid.Pe_loc*Param.c_in(ifield) + x(n) ) / (1 + Grid.Pe_loc);
            f(n) = f(n) + A_lo*c_lo;
            Jac(n,n) = Jac(n,n) + A_lo/(1+Grid.Pe_loc);
        end
        
        % dependence on value to the right
        if(k < Grid.N)   % if not the right-most point
            m = tubular_reactor_get_label(ifield,k+1,Param.N_f);  % value of same field at right point
            f(n) = f(n) + A_hi*x(m);
            Jac(n,m) + Jac(n,m) + A_hi;
        else  % use outlet BC to estimate conc. at outlet
            c_hi = conc_loc(ifield);  % same as current value
            f(n) = f(n) + A_hi*c_hi;
            Jac(n,n) = Jac(n,n) + A_hi;
        end
    end    
end

return;



% ----------------------------------------------------------------------
% scheme_2rxn.m
% This routine computes the rates of each reaction, sets the stoichiometric
% coefficients, and computes the derivative of each reaction rate with
% respect to the concentrations of each species.
function [num_rxn,rxn_rate,stoich_coeff,rxn_rate_deriv] = ...
        scheme_2rxn(conc_loc,Param);

% compute rates of each reaction and set stiochoimetric coefficients
num_rxn = length(Param.k);
rxn_rate = zeros(1,num_rxn);
stoich_coeff = zeros(Param.N_f,num_rxn);
% reaction 1, A + B --> C,  (1) + (2) --> (3)
rxn_rate(1) = Param.k(1)*conc_loc(1)*conc_loc(2);  % local rate of rxn. 1
stoich_coeff(:,1) = [-1; -1; 1; 0];
% reaction 2, B + C --> D,  (2) + (3) --> (4)
rxn_rate(2) = Param.k(2)*conc_loc(2)*conc_loc(3);  % local rate of rxn. 2
stoich_coeff(:,2) = [0; -1; -1; 1];
        
% compute partials of each rxn. rate with respect to concentrations.
rxn_rate_deriv = zeros(Param.N_f,num_rxn);
% reaction 1, A + B --> C,  (1) + (2) --> (3)
rxn_rate_deriv(1,1) = Param.k(1)*conc_loc(2);  % deriv. of rate # 1 w.r.t. [A]
rxn_rate_deriv(2,1) = Param.k(1)*conc_loc(1);  % deriv. of rate # 1 w.r.t. [B]
% reaction 2, B + C --> D,  (2) + (3) --> (4)
rxn_rate_deriv(2,2) = Param.k(2)*conc_loc(3);  % deriv. or rate # 2 w.r.t. [B]
rxn_rate_deriv(3,2) = Param.k(2)*conc_loc(2);  % deriv. of rate # 2 w.r.t. [C]

return;



% ==================================================================
% Save this routine to tubular_reactor_get_label.m
% This routine returns the index that identifies the location in the
% state vector of the concentation at a specific interior grid point
function label = tubular_reactor_get_label(ifield,k,N_f);

label = N_f * (k - 1) + ifield;

return;



% ---------------------------------------------------------------------
% extract_field.m
% This routine extracts from the state vector the concentration values
% for a particular species from the master state vector.  It also uses
% the Danckwert's BC to compute the inlet and outlet concentrations.
function field_val = extract_field(ifield,x,Grid,Param);

field_val = zeros(Grid.N + 2, 1);  % allocate space in memory

for k=1:Grid.N
    n = tubular_reactor_get_label(ifield,k,Param.N_f);
    field_val(1+k) = x(n);
end

% add outlet value from BC
field_val(Grid.N + 2) = field_val(Grid.N + 1);

% add inlet value from BC
field_val(1) = ( Grid.Pe_loc * Param.c_in(ifield) + field_val(2) ) / (1 + Grid.Pe_loc);

return;
