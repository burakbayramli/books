% sim_anneal_MRSL.m
% [theta, F_cost, det_S] = sim_anneal_MRSL(...
%       MRSLData, theta_0, N_iter, ...
%       freq_quench, freq_reset, T_0);
%
% This routine performs a simulated annealing run to estimate
% the parameters using a composite data set.
%
% The input arguments are:
% MRSLData: A data structure that contains the following
% fields:
% .num_sets = the number of response data sets in the
%             composite set of data
% .P = the number of parameters to be fitted
% .M = a vector containing for each data set the number
%      of predictor variables
% .L = a vector containing for each data set the number
%      of response variables
% .N = a vector containing for each data set the number
%      of experiments
% .X_pred_j = for each data set j, the N x M matrix of
%      predictor values
% .Y_j = for each data set j, the N x L matrix of
%      measured response values
% .fun_yhat_j = for each data set j, the name of a routine
%      that predicts the response values,
%        Y_hat = feval(fun_yhat_j, theta, X_pred_j);
%
% theta_0 = the initial guess of the parameter vector
%
% N_iter = the number of MC interations during the
%          simulated annealing run
%
% freq_quech = the fraction of iterations in which the
%          simulation is quenched, i.e. minimized with
%          fminsearch, to move towards a local minimum.
%          If non-zero, a local minimization is performed
%          again at the end of the simulated annealing run.
%          The default value is zero.
%
% freq_reset = the fraction of iterations in which at random
%          theta is reset to the best value identified to date.
%
% T_0 = the initial "temperature" of the annealing run.
%          If not present as an argument, a default value
%          equal to 10 times the initial magnitude of the
%          cost function.
%
% The output arguments are:
% theta = the final estimate of the parameter vector
% F_cost = the cost function value at theta
% det_S = a vector of the det(S) values for each
%         data set
% K. J. Beers. MIT ChE. 12/17/2004

function [theta, F_cost, det_S] = sim_anneal_MRSL(...
            MRSLData, theta_0, N_iter, ...
            freq_quench, freq_reset, T_0);

% First, compute the initial response predictions, S
% matrix, and cost function value for the initial guess.
theta = theta_0;
[F_cost, det_S] = calc_MRSL_posterior(theta, MRSLData);
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

% -- if freq_quench is non-zero, begin with computation
% of local minimum
theta = fminsearch('calc_MRSL_posterior', ...
    theta, [], MRSLData);
[F_cost, det_S] = calc_MRSL_posterior(...
    theta, MRSLData);

% - - - - -
% Begin the simulated annealing run
for iter = 1:N_iter

    % decide at random whether to perform quench
    if(rand <= freq_quench)
        theta = fminsearch('calc_MRSL_posterior', ...
            theta, [], MRSLData);
        [F_cost, det_S] = calc_MRSL_posterior(...
            theta, MRSLData);
    end
        
    % compute current temperature based on
    % linear cooling rate
    T = T_0 * (1 - (iter-1)/N_iter);
    
    % propose theta move
    theta_new = theta + ...
        theta_scale_rel.*avg_mag.* ...
        randn(MRSLData.P,1);

    % compute cost function at the new value
    [F_cost_new, det_S_new] = calc_MRSL_posterior(...
        theta_new, MRSLData);
    
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
    theta = fminsearch('calc_MRSL_posterior', ...
        theta, [], MRSLData);
    [F_cost, det_S] = calc_MRSL_posterior(...
        theta, MRSLData);
end

return;


