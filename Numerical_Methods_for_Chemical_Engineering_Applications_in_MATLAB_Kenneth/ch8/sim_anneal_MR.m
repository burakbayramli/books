% sim_anneal_MR.m
% [theta, det_S, det_S_0] = sim_anneal_MR(...
%       X_pred, Y, fun_yhat, theta_0, N_iter, ...
%       freq_quench, freq_reset, T_0);
%
% This routine performs a simulated annealing run to estimate
% the parameters of a model that describes a multi-response
% data set. Here, we use as the cost function the determinant
% of the positive-definite S matrix.
%
% N = number of experiments
% M = number of predictors
% P = number of parameters
% Y = number of responses per experiment
%
% The input arguments are:
% X_pred = NxM matrix in which each of the N rows contains
%          the values of the M predictors for the
%          corresponding experiment
% Y = NxL matrix of response data, in which each of the N
%          rows contains the L response values for the
%          corresponding experiment
% fun_yhat = name of a routine that returns the matrix
%          of predicted responses from input values of
%          theta (the vector of P parameters) and
%          X_pred (the matrix of predictor values)
% theta_0 = the initial guess of the parameter vector
% N_iter = the number of MC interations during the
%          simulated annealing run
% freq_quech = the fraction of iterations in which the
%          simulation is quenched, i.e. minimized with
%          fminsearch, to move towards a local minimum.
%          If non-zero, a local minimization is performed
%          again at the end of the simulated annealing run.
%          The default value is zero.
% freq_reset = the fraction of iterations in which at random
%          theta is reset to the best value identified to date.
% T_0 = the initial "temperature" of the annealing run.
%          If not present as an argument, a default value
%          equal to 10 times the initial value of det_S
%          is used
%
% The output arguments are:
% theta = the final estimate of the parameter vector
% det_S = the value of the determinant of the S
%         matrix at the final estimate
% det_S_0 = the value of the determinate of the S matrix
%        at the initial guess
% K. J. Beers. MIT ChE. 12/15/2004


function [theta, det_S, det_S_0] = sim_anneal_MR(...
    X_pred, Y, fun_yhat, theta_0, N_iter, ...
    freq_quench, freq_reset, T_0);

% extract dimensioning parameters
N = size(Y,1);
L = size(Y,2);
M = size(X_pred,2);
P = length(theta_0);

% First, compute the initial response predictions, S
% matrix, and cost function value for the initial guess.
theta = theta_0;
[F_cost, det_S] = ...
    calc_Fcost_sim_anneal_MR(theta, X_pred, Y, fun_yhat);
F_cost_0 = F_cost;  det_S_0 = det_S;

% set the initial temperature to the default
% value if necessary
try
    val = T_0;
catch
    T_0 = 10*abs(F_cost_0);
    if(T_0 < sqrt(eps))
        T_0 = sqrt(eps);
    end
end

% set the frequency of quenching to the default value
try
    val = freq_quench;
catch
    freq_quench = 0;
end

% set relative scale of theta moves
theta_scale_rel = 1e-2;

% initialize sum that keeps track of the
% average magintudes of each parameter
sum_mag = abs(theta_0);
avg_mag = sum_mag;

% keep track of the best theta encountered
theta_best = theta;
F_cost_best = F_cost;  det_S_best = det_S;

% - - - - -
% Begin the simulated annealing run
for iter = 1:N_iter

    % decide at random whether to perform quench
    if(rand <= freq_quench)
        theta = fminsearch(@calc_Fcost_sim_anneal_MR, ...
            theta, [], X_pred, Y, fun_yhat);
        [F_cost, det_S] = ...
            calc_Fcost_sim_anneal_MR(theta, X_pred, Y, fun_yhat);
    end
        
    
    % compute current temperature based on
    % linear cooling rate
    T = T_0 * (1 - (iter-1)/N_iter);
    
    % propose theta move
    theta_new = theta + ...
        theta_scale_rel.*avg_mag.* ...
        randn(P,1);
    
    % compute det(S) at new value
    [F_cost_new, det_S_new] = ...
        calc_Fcost_sim_anneal_MR( ...
            theta_new, X_pred, Y, fun_yhat);
    
    % compute acceptance probability
    delta_F_cost = F_cost_new - F_cost;
    prob_accept = min(1,exp(-delta_F_cost/T));
    
    % decide whether to accept move or not
    if(rand <= prob_accept)
        % accept move
        theta =  theta_new;
        det_S = det_S_new;
        F_cost = F_cost_new;
    end
    
    % compute new average magintudes of
    % parameters
    sum_mag = sum_mag + abs(theta);
    avg_mag = sum_mag/(iter+1);
    
    % check to see if current state is best one
    if(F_cost < F_cost_best)
        theta_best = theta;
        det_S_best = det_S;
        F_cost_best = F_cost;
    end
    
end


% report best result, and do final local
% minimization if freq_quench is non-zero
theta = theta_best;
det_S = det_S_best;
F_cost = F_cost_best;
if(~freq_quench)
    theta = fminsearch(@calc_Fcost_sim_anneal_MR, ...
        theta, [], X_pred, Y, fun_yhat);
    [F_cost, det_S] = ...
        calc_Fcost_sim_anneal_MR(theta, X_pred, Y, fun_yhat);
end

return;


% ====================================
% This routine computes the cost function value for the
% multi-response regression problem.
function [F_cost, det_S] = ...
    calc_Fcost_sim_anneal_MR(theta, X_pred, Y, fun_yhat);

Y_hat = feval(fun_yhat, theta, X_pred);
Delta = Y - Y_hat;
S = Delta'*Delta;
det_S = abs(det(S));
F_cost = log(max(eps, det_S));

return;

