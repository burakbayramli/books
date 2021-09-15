% Ising_lattice_MC.m
% This MATLAB program performs Monte Carlo simulation on an
% 2-D Ising lattice.
% Input:
% MCOpts - a data structure with members
%    .N = # of sites in each direction
%    .mu = site magnetization
%    .H = external magnetic field
%    .J = coupling constant
%    .kb_T = thermal energy
%    .S_init = initial site spins (as 2D array)
%    .Nequil = # equilibration MC steps
%    .Nsamples = # total number of samples
%    .freq_sample = # MC steps between samples
%    .make_plots = if non-zero, plot trajectories and final state
%
% Output:
%--------
% m_PROPS - data structure with magnetization information
%     .avg = average net magnetization
%     .var = variance of net magnetization
%     .order = 0 if no order, +1 if all + spins, -1 if all - spins
%     .traj = trajectory of net magnetization
% ener_PROPS - data structure with energy information
%     .avg = average energy
%     .var = variance of energy
%     .traj = trajectory of energy
% S = final state spins
%
% K. Beers. MIT ChE. 11/7/03
% v 11/14/03.A. corrected bug in magnetization definition
% v 12/23/03.A. added optional display of progress to screen
function [mag_PROPS,ener_PROPS,S,iflag_main] = Ising_lattice_MC(MCOpts);
iflag_main = 0;

% Extract simulation parameters from MCOpts and use defaults
% display
try
    display = MCOpts.display;
catch
    display = 0;
end
% Nequil
try
    Nequil = MCOpts.Nequil;
catch
    Nequil = 1e5;
end
if(display)
    Nequil,
end
% Nsamples
try
    Nsamples = MCOpts.Nsamples;
catch
    Nsamples = 1e5;
end
if(display)
    Nsamples,
end
% freq_sample
try
    freq_sample = MCOpts.freq_sample;
catch
    freq_sample = 10;
end
if(display)
    freq_sample,
end
% N
try
    N = MCOpts.N;
catch
    N = 100;
end
if(display)
    N,
end
% mu
try
    mu = MCOpts.mu;
catch
    mu = 1;
end
if(display)
    mu,
end
% H
try
    H = MCOpts.H;
catch
    H = 0;
end
if(display)
    H,
end
% J
try
    J = MCOpts.J;
catch
    J = 1;
end
if(display)
    J,
end
% kb_T
try
    kb_T = MCOpts.kb_T;
catch
    kb_T = 1;
end
if(display)
    kb_T,
end
% make_plots
try
    make_plots = MCOpts.make_plots;
catch
    make_plots = 0;
end
if(display)
    make_plots,
end

% Next, set initial spin states (use random dist. if no initial state input)
try 
    S = MCOpts.S_init;
catch
    S = rand(N,N);
    for i=1:N
        for j=1:N
            if(S(i,j) <= 0.5)
                S(i,j) = 1;
            else
                S(i,j) = -1;
            end
        end
    end
end

% Next, compute energy and magnetization of initial state
[ener,mag] = Ising_lattice_calc_ener(S,N,mu,H,J);


% ---------------------------
% Perform equilibration stage
if(display)
    disp(' '); disp('Beginning equilibration ...');
end
for iMC = 1:Nequil
    
    % every so often, recompute the total energy to avoid creeping errors
    if(~mod(iMC,N))
        [ener,mag] = Ising_lattice_calc_ener(S,N,mu,H,J);
    end
    if(display)
        if(~mod(iMC,10000))
            disp(['     Passing iMC = ', int2str(iMC)]);
        end
    end
    
    % generate new state by randomly changing one spin
    % and compute delta in system energy
    [i_change,j_change,delta_ener,delta_mag] = propose_MC_move(S,N,mu,H,J);
    
    % compute acceptance probability and see whether to accept move
    prob_ratio = exp(-delta_ener/kb_T);
    alpha = min(1, prob_ratio);
    rand_var = rand;  % random variable uniformly distributed on [0,1]
    if(rand_var <= alpha)  % accept move
        S(i_change,j_change) = -S(i_change,j_change);
        ener = ener + delta_ener;
        mag = mag + delta_mag;
    end
    
end


% --------------------
% Begin sampling stage
if(display)
    disp('Beginning sampling ...');
end

% allocate space for magnetization trajectory
mag_PROPS.traj = zeros(Nsamples,1);
ener_PROPS.traj = zeros(Nsamples,1);

for isample = 1:Nsamples
    
    % compute new energy to avoid creeping error
    % in energy and magnetization
    [ener,mag] = Ising_lattice_calc_ener(S,N,mu,H,J);

    if(display)
        if(~mod(isample,1000))
            disp(['     Passing isample = ', int2str(isample)]);
        end
    end

    
    
    % perform some number of MC steps between samples
    for iMC = 1:freq_sample
        % generate new state by randomly changing one spin
        % and compute delta in system energy
        [i_change,j_change,delta_ener,delta_mag] = ...
            propose_MC_move(S,N,mu,H,J);
        
        % compute acceptance probability and see whether to accept move
        prob_ratio = exp(-delta_ener/kb_T);
        alpha = min(1, prob_ratio);
        rand_var = rand;  % random variable uniformly distributed on [0,1]
        if(rand_var <= alpha)  % accept move
            S(i_change,j_change) = -S(i_change,j_change);
            ener = ener + delta_ener;
            mag = mag + delta_mag;
        end
    end
    % record current result in trajectory
    ener_PROPS.traj(isample) = ener;
    mag_PROPS.traj(isample) = mag;
    
end

% ----------------------------------------------------------
% compute statistical properties of magnetization and energy
mag_PROPS.avg = mean(mag_PROPS.traj);
mag_PROPS.var = var(mag_PROPS.traj);
mag_PROPS.order = mag_PROPS.avg / (N^2) / mu;
ener_PROPS.avg = mean(ener_PROPS.traj);
ener_PROPS.var = var(ener_PROPS.traj);

% ---------------------------------------------------------
% if desired, make plots of final state and the trajectory
% information of the magnetization and energy
if(make_plots)    
    
    % final state space
    figure;
    spy(S + ones(size(S)));
    xlabel('lattice x-site #i');  ylabel('lattice y-site #j');
    title_phrase = ['Ising lattice + spins: ', ...
            'N = ', int2str(N), ...
            ', \mu = ', num2str(mu), ...
            ', H = ', num2str(H), ...
            ', J = ', num2str(J), ...
            ', k_bT = ', num2str(kb_T)];
    title(title_phrase);
   
    % magnetization properties
    figure;
    subplot(2,1,1);
    plot(mag_PROPS.traj);
    title_phrase = ['Ising lattice magnetization : ', ...
            'N = ', int2str(N), ...
            ', \mu = ', num2str(mu), ...
            ', H = ', num2str(H), ...
            ', J = ', num2str(J), ...
            ', k_bT = ', num2str(kb_T)];
    xlabel('MC sample');  ylabel('m');
    title(title_phrase);
    subplot(2,1,2);
    hist(mag_PROPS.traj/Nsamples,20);
    xlabel('m');  ylabel('Pr(m)');
    title(['avg. = ', num2str(mag_PROPS.avg),', variance = ', num2str(mag_PROPS.var), ...
            ', order param = ', num2str(mag_PROPS.order)]);
    
    % energy properties
    figure;
    subplot(2,1,1);
    plot(ener_PROPS.traj);
    title_phrase = ['Ising lattice energy : ', ...
            'N = ', int2str(N), ...
            ', \mu = ', num2str(mu), ...
            ', H = ', num2str(H), ...
            ', J = ', num2str(J), ...
            ', k_bT = ', num2str(kb_T)];
    xlabel('MC sample');  ylabel('energy');
    title(title_phrase);
    subplot(2,1,2);
    hist(ener_PROPS.traj/Nsamples,20);
    xlabel('E');  ylabel('Pr(E)');
    title(['avg. = ', num2str(ener_PROPS.avg),', variance = ', num2str(ener_PROPS.var)]);

end   


iflag_main = 1;
return;


% ===============================================
% This routine computes the total system energy
% and net magnetization of the 2-D Ising lattice
function [ener,mag] = Ising_lattice_calc_ener(S,N,mu,H,J);

% First, compute energy due to coupling with external
% field
mag = mu*sum(sum(S));  % net magnetization
ener_ext_H = H*mag;

% Next, compute energy associated with coupling
% between each state
sum1 = 0;
for i=1:N
    for j=1:N
        % (i-1,j)
        m = i-1;
        if(m==0)  % enforce periodic BC
            m = N;
        end
        n = j;
        sum1 = sum1 + S(i,j)*S(m,n);
        % (i+1,j)
        m = i+1;
        if(m==(N+1))
            m = 1;
        end
        n = j;
        sum1 = sum1 + S(i,j)*S(m,n);
        % (i,j-1)
        m = i;
        n = j-1;
        if(n==0)
            n = N;
        end
        sum1 = sum1 + S(i,j)*S(m,n);
        % (i,j+1)
        m = i;
        n = j+1;
        if(n==(N+1))
            n = 1;
        end
        sum1 = sum1 + S(i,j)*S(m,n);
    end
end
ener_J_couple = -J/2*sum1;

% get total system energy
ener = ener_ext_H + ener_J_couple;

return;


% ==================================
% This routine selects at random a spin to change and
% computes the energy of doing so.
function [i_change,j_change,delta_ener,delta_mag] = ...
    propose_MC_move(S,N,mu,H,J);

i_change = round(  1 + rand*(N-1));
j_change = round(  1 + rand*(N-1));

% compute current and new energies of this site interacting
% with its neighbors
sum1 = 0;
sum1_new = 0;

% (i_change-1,j_change)
m = i_change-1;
if(m==0)  % enforce periodic BC
    m = N;
end
n = j_change;
sum1 = sum1 + S(i_change,j_change)*S(m,n);
sum1_new = sum1_new - S(i_change,j_change)*S(m,n);
% (i_change+1,j_change)
m = i_change+1;
if(m==(N+1))
    m = 1;
end
n = j_change;
sum1 = sum1 + S(i_change,j_change)*S(m,n);
sum1_new = sum1_new - S(i_change,j_change)*S(m,n);
% (i_change,j_change-1)
m = i_change;
n = j_change-1;
if(n==0)
    n = N;
end
sum1 = sum1 + S(i_change,j_change)*S(m,n);
sum1_new = sum1_new - S(i_change,j_change)*S(m,n);
% (i_change,j_change+1)
m = i_change;
n = j_change+1;
if(n==(N+1))
    n = 1;
end
sum1 = sum1 + S(i_change,j_change)*S(m,n);
sum1_new = sum1_new - S(i_change,j_change)*S(m,n);

ener = -J*sum1;
ener_new = -J*sum1_new;
delta_ener = ener_new - ener;

% Than, add change in energy in external field and get
% total magnetization
delta_mag = -2*mu*S(i_change,j_change);
delta_ener = delta_ener + H*delta_mag;


return;
