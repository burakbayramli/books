% BD_1D.m
% This MATLAB program performs Brownian dynamics
% simulation of a single particle in a potential
% field U(x). A simple explicit Euler method is
% used to propagate the Langevin SDE.
% K. Beers. MIT ChE. 10/27/03
function iflag_main = BD_1D(t_end,dt);
iflag_main = 0;

% First, set the parameters
D = 1;  % diffusivity
zeta = 1;  % drag coefficient
k_sp = 1;  % spring constant for potential energy

% Next, set the vector of time values at which
% we compute the position of the particle.
t_vect = [0:dt:t_end];  num_t_val = length(t_vect);
x_vect = zeros(size(t_vect));

% perform BD iterations to compute trajectory,
% and periodically report result
x = 0;
var_sqrt_2D = sqrt(2*D);  var_sqrt_dt = sqrt(dt);
for iter=1:num_t_val
    x_vect(iter) = x;
    % compute deterministic force from Hookian spring
    F_spring = -k_sp*x;    
    % compute increment of Wiener process
    dWt = var_sqrt_dt*randn;
    % compute BD update
    x = x + F_spring/zeta*dt + var_sqrt_2D*dWt;
end

% Now, plot random trajectory
figure;  plot(t_vect,x_vect);
xlabel('t');  ylabel('x(t)');
title_phrase = ['Brownian dynamics of particle in 1-D quadratic well', ...
        ', \Deltat = ', num2str(dt)];
title(title_phrase);

% Now, we compute the histogram of the trajectory, and plot this in
% comparison to the Boltzmann distribution with kT = 1
figure; num_bins = 50;
[n_hist,x_hist] = hist(x_vect,num_bins);
var1 = trapz(x_hist,n_hist);  n_hist = n_hist ./ var1;
bar(x_hist,n_hist);
x_plot = linspace(-3,3,500);  V_plot = 0.5*k_sp.*(x_plot.^2);
kb_T = 1;  p_Boltzmann = exp(-V_plot/kb_T);
var1 = trapz(x_plot,p_Boltzmann);  p_Boltzmann = p_Boltzmann ./ var1;
hold on;  plot(x_plot,p_Boltzmann);
xlabel('x');  ylabel('p(x)');
title('Probability distribution function of x for 1-D Brownian motion');

iflag_main = 1;
return;
