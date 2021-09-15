% calc_Yhat_reaction_ex_dynamic_MR.m

function Y_hat = calc_Yhat_reaction_ex_dynamic_MR(k1, X_pred);

t_span = X_pred'; % set times at which to report concentrations
x_0 = [0.1; 0.1; 0]; % set initial condition

% perform dynamic simulation
[t_traj,x_traj] = ode45(@batch_kinetics_dynamics_ex, ...
    t_span, x_0, [], k1);

% extract response predictions
Y_hat = x_traj;

return;

