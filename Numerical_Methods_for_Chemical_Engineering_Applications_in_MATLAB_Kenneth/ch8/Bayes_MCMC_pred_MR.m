% Bayes_MCMC_pred_MR.m
%
% function g_pred = Bayes_MCMC_pred_MR(X_pred, Y, ...
%         fun_yhat, fun_g, theta_0, MCOPTS, Param);
%
% This routine computes the expectation of a vector g, given a
% set of multi-response data, using Markov Chain Monte Carlo
% (MCMC) simulation.
%
% The user provides a multi-response data set for N experiments,
% by entering X_pred, a matrix in which row k contains the
% values of the predictor variables for experiment # k.
% Y is the response data matrix, whose row k contains the L
% measured response values for experiment k.
% The user also provides the name fun_yat of a routine
% that returns the matrix of predicted responses as a
% function of the model parameters theta and the matrix of
% predictor values in each experiment, X_pred:
%     Y_hat = feval(fun_yhat, theta, X_pred);
%
% From this mutli-response data set, the marginal posterior
% density for theta is specified to within an additive
% constant. Markov Chain Monte Carlo (MCMC) simulation is
% then performed to compute the expectation of some
% vector g(theta).
%
% The user supplies the name of a routine fun_g that computes
% the value of some function g(theta),
%           g = feval(fun_g, theta, Param);
% where Param is some optional set of parameters passed to fun_g
% through this routine.
%
% MCOPTS controls the details of the MCMC simulation, and
% contains the fields (and default values):
%  .N_equil = the number of equilibration MC iterations to be
%             performed before sampling begins (1,000)
%  .N_samples = the number of MC iterations run during sampling.
%             The larger this number, the more accurate the
%             prediction. (10,000)
% .delta_theta = the size of the displacements in each
%             parameter during a proposed MC move relative
%             to average magnitudes of each parameter (0.1).
%             This value is changed to meet the target
%             fraction of accepted moves.
% .accept_tar = target fraction of proposed MC moves
%             that are accepted (0.3).
%
% Kenneth J. Beers
% MIT ChE. 12/15/2004. ver. 12/15/2004

function g_pred = Bayes_MCMC_pred_MR(X_pred, Y, ...
    fun_yhat, fun_g, theta_0, MCOPTS, Param);

routine_name = 'Bayes_MCMC_pred_MR';
num_arg_in = nargin;

% extract dimension of problem
P = length(theta_0);
M = size(X_pred,2); % # of predictors
N = size(Y,1);  % # of experiments
L = size(Y,2); % # of responses

% add default values to MCOPTS as needed
% .N_equil
try
    val = MCOPTS.N_equil;
    if(val < 0)
        error([routine_name, ...
                ': MCOPTS.N_equil < 0']);
    end
catch
    MCOPTS.N_equil = 1000;
end

%  .N_samples
try
    val = MCOPTS.N_samples;
    if(val < 0)
        error([routine_name, ...
                ': MCOPTS.N_samples < 0']);
    end
catch
    MCOPTS.N_samples = 10000;
end        
% .delta_theta
try
    val = MCOPTS.delta_theta;
catch
    MCOPTS.delta_theta = 0.1;
end
    
% .accept_tar
try
    val = MCOPTS.accept_tar;
    if(or((val<=0),(val>=1)))
        error([routine_name, ...
                ': MCOPTS.accept_tar out of (0,1)']);
    end

catch
    MCOPTS.accept_tar = 0.3;
end

% Now, we initialize the counters that keep track of the
% acceptance fraction and the numbers of samples taken
num_theta_try = 0;
num_theta_accept = 0;
num_samples_taken = 0;

% initialize the running sums of the magnitudes of the
% parameters, and the values avg_mag used in setting
% the step sizes for each parameter.
sum_mag = abs(theta_0);
avg_mag = sum_mag;
k_small = find(avg_mag < sqrt(eps));
avg_mag(k_small) = sqrt(eps);

% with initial theta_0, sigma_0, compute the
% initial vector of predictions, sum of squared
% errors, and posterior probability.
theta = theta_0;
Y_hat = feval(fun_yhat, theta, X_pred);
Delta = Y - Y_hat;
S = Delta'*Delta;
det_S = abs(det(S));
posterior_density = det_S^(-N/2); 

% allocate space to store prediction of g
% and set to initially contain zeros
if(nargin >= 7)
    g_pred = feval(fun_g,theta,Param);
else
    g_pred = feval(fun_g,theta);
end
g_pred = zeros(size(g_pred));

% begin Monte Carlo iterations
delta_theta = MCOPTS.delta_theta;
N_iter_tot = MCOPTS.N_equil + MCOPTS.N_samples;
for iter_MC = 1 : N_iter_tot

    % occasionally, adjust step sizes to fit
    % target acceptance fraction if we have
    % collected enough data since last
    % adjustment. If the acceptance fraction
    % is too low, decrease the step size of
    % the moves.  If too high, increase the
    % step size of the moves.
    if(num_theta_try > 500)
        frac_accept = ...
            num_theta_accept/num_theta_try;
        if(frac_accept < MCOPTS.accept_tar)
            delta_theta = delta_theta * 0.9;
        elseif(frac_accept > MCOPTS.accept_tar)
            delta_theta = delta_theta * 1.1;
        end
        % reset counters
        num_theta_try = 0;
        num_theta_accept = 0;
    end

    % propose theta move
    theta_new = theta + ...
        delta_theta.*avg_mag.* ...
        randn(size(theta));
    num_theta_try = num_theta_try + 1;
    
    % compute predictions with new theta, and
    % obtain new posterior density
    Y_hat_new = feval(fun_yhat,theta_new,X_pred);
    Delta_new = Y - Y_hat_new;
    S_new = Delta_new'*Delta_new;
    det_S_new = abs(det(S_new));
    posterior_density_new = det_S_new^(-N/2);
        
    % compute the acceptance probability
    accept_prob = posterior_density_new/ ...
        posterior_density;
    if(accept_prob > 1)
        accept_prob = 1;
    end
    
    % decide whether to accept the move or not
    u = rand;
    if(u <= accept_prob)  % accept the move
        theta = theta_new;
        Y_hat = Y_hat_new;
        Delta = Delta_new;
        S = S_new;
        det_S = det_S_new;
        posterior_density = posterior_density_new;
        num_theta_accept = num_theta_accept + 1;
    end
            
    % if we are in the sampling stage, then measure the function
    % g(theta,sigma) and add to running sum.
    if(iter_MC > MCOPTS.N_equil)
        if(nargin >= 7)
            g_meas = feval(fun_g,theta,Param);
        else
            g_meas = feval(fun_g,theta);
        end
        g_pred = g_pred + g_meas;
        num_samples_taken = num_samples_taken + 1;
    end
    
    % increment the values of sum_mag and avg_mag
    sum_mag = sum_mag + abs(theta);
    avg_mag = sum_mag/(iter_MC+1);
    k_small = find(avg_mag < sqrt(eps));
    avg_mag(k_small) = sqrt(eps);
    
end

% get prediction from sum of measured predictions
g_pred = g_pred/num_samples_taken;


return;