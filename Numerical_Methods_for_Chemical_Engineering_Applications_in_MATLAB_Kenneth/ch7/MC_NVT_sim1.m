% MC_NVT_sim1.m
% This MATLAB program performs NVT Monte Carlo
% simulation of a single particle in a potential
% field U(x).
% The inputs are:
% N_eq, the number of equilibration MC moves
% N_samples, the number of samples
% freq_sample, the number of MC steps between samples
% dx_max, the max. displacement during a MC move
% K. Beers. MIT ChE. 10/28/03
function iflag_main = MC_NVT_sim1(N_eq,N_samples,freq_sample,dx_max);
iflag_main = 0;

k_sp = 1;  % set spring constant value
kb_T = 1;  % value of thermal energy

% First, perform an equilirbiation run
x = 0;  % set initial state
U_pot = 0.5*k_sp*x^2;  % current poential energy
prob = exp(-U_pot / kb_T);  % calculate Boltzmann weight
for iter_MC = 1:N_eq    
    % propose MC move (random displacement)
    rand_var = rand;
    dx_rand = dx_max*(rand_var - 0.5);
    x_new = x + dx_rand;  % proposed new state
    % calculate potential energy and Boltzmann weight of new state
    U_pot_new = 0.5*k_sp*x_new^2;
    prob_new = exp(-U_pot_new / kb_T);    
    % calculate acceptance probability from Metropolis rule
    alpha = min(1, prob_new/prob);    
    % check to see if we accept MC move
    rand_var = rand;
    if(rand_var <= alpha)  % accept move
        x = x_new;
        U_pot = U_pot_new;  prob = prob_new;
    end    
end    

% ----- Now, we perform sampling run.
% allocate space to store sampled values
x_traj = zeros(1,N_samples);  count_samples = 0;
num_MC_steps = round(1.1*N_samples*freq_sample);
% perform sampling MC simulation
for iter_MC=1:num_MC_steps
    % check to see if it is time to report results
    if(~mod(iter_MC,freq_sample))
        count_samples = count_samples + 1;
        x_traj(count_samples) = x;
    end    
    % end simulation when # of samples is performed
    if(count_samples >= N_samples)
        break;
    end
    % propose MC move (random displacement)
    rand_var = rand;
    dx_rand = dx_max*(rand_var - 0.5);
    x_new = x + dx_rand;  % proposed new state    
    % calculate potential energy and Boltzmann weight of new state
    U_pot_new = 0.5*k_sp*x_new^2;
    prob_new = exp(-U_pot_new/kb_T);    
    % calculate acceptance probability from Metropolis rule
    alpha = min(1, prob_new/prob);    
    % check to see if we accept MC move
    rand_var = rand;
    if(rand_var <= alpha)  % accept move
        x = x_new;
        U_pot = U_pot_new;  prob = prob_new;
    end    
end

% Now, plot sampled MC trajectory
figure;  plot(x_traj);
xlabel('MC sample');  ylabel('x');
title('NVT-MC trajectory of particle in 1-D quadratic energy well');

% Now, we compute the histogram of the trajectory, and plot this in
% comparison to the Boltzmann distribution with kT = 1
figure; num_bins = 50;
[n_hist,x_hist] = hist(x_traj,num_bins);
var1 = trapz(x_hist,n_hist);  n_hist = n_hist ./ var1;
bar(x_hist,n_hist);
x_plot = linspace(-3,3,500);  V_plot = 0.5*k_sp.*(x_plot.^2);
kb_T = 1;  p_Boltzmann = exp(-V_plot/kb_T);
var1 = trapz(x_plot,p_Boltzmann);  p_Boltzmann = p_Boltzmann ./ var1;
hold on;  plot(x_plot,p_Boltzmann);
xlabel('x');  ylabel('p(x)');
title('NVT probability distribution of particle in 1-D quadratic energy well');
% add text with simulation parameters
gtext({  ['N_{eq} = ', int2str(N_eq)] , ...
    ['N_{samples} = ', int2str(N_samples)], ...
    ['sampling freq = ', int2str(freq_sample)], ...
    ['\Deltax_{max} = ', num2str(dx_max)] } );
    
iflag_main = 1;
return;
