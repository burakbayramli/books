% Bayes_MCMC_pred_SR.m
%
% function g_pred = Bayes_MCMC_pred_SR(X_pred, y, ...
%         fun_yhat, fun_g, theta_0, sigma_0, MCOPTS, Param);
%
% This routine computes the expectation of a vector g, given a
% set of single response data, using Markov Chain Monte Carlo
% (MCMC) simulation.
%
% The user provides a single-response data set for N experiments,
% by entering X_pred, a matrix in which row k contains the
% values of the predictor variables for experiment # k and y,
% the N-dimensional vector of measured responses in each
% experiment. The user also gives the name fun_yat of a routine
% that returns the vector of predicted responses as a
% function of the model parameters theta and the matrix of
% predictor values in each experiment, X_pred:
%     y_hat = feval(fun_yhat, theta, X_pred);
%
% Given this single-response data, the posterior probability
% density p(theta,sigma| y) is determined (within a
% normalization factor), assuming a non-informative prior.
% This characterizes the probability, based on the data,
% that the model of the system has a parameter vector theta
% and that the random error present in the measured response
% data has a standard deviation sigma.  This posterior density
% assumes that the errors in each experiment have averages of
% zero, that errors in different experiments are indpendent,
% and that the errors in each experiment have the same variance.
% In addition, the errors are assumed to be normally distributed.
%
% The user then supplies the name of a routine fun_g that computes
% the value of some function g(theta,sigma),
%           g = feval(fun_g, theta, sigma);
% This routine then uses the Metropolis algorithm to generate,
% starting at input values (theta_0, sigma_0), a Markov chain of
% (theta,sigma) values drawn from posterior density.
% The average of g(theta,sigma) over this Markov chain is
% reported as the estimated posterior prediction of g.
%
% MCOPTS controls the details of the MCMC simulation, and
% contains the fields (and default values):
%  .N_equil = the number of equilibration MC iterations to be
%             performed before sampling begins (1,000)
%  .N_samples = the number of MC iterations run during sampling.
%             The larger this number, the more accurate the
%             prediction. (10,000)
%  .frac_theta = the fraction of MC moves that involve
%             displacing theta (0.5).  The remainder of the
%             moves displace sigma.
% .delta_theta = the size of the displacements in each
%             parameter during a proposed MC move (0.1 of
%             infinity norm of theta_0).
%             This value is changed to meet the target
%             fraction of accepted moves.
% .delta_sigma = the size of the displacement during
%             a proposed MC move is sigma. (0.1 of
%             sigma_0). This value is changed to meet
%             the target fraction of accepted moves.
% .accept_tar = target fraction of proposed MC moves
%             that are accepted (0.3).
%
% Param is an optional set of parameters that is passed
% to fun_g
%
% Kenneth J. Beers
% MIT ChE. 12/8/2004.
% ver 7/19/2005, modified to use Gaussian displacements
% ver 8/24/2005, modified to improve round-off errors

function g_pred = Bayes_MCMC_pred_SR(X_pred, y, ...
    fun_yhat, fun_g, theta_0, sigma_0, MCOPTS, Param);

routine_name = 'Bayes_MCMC_pred_SR';
num_arg_in = nargin;

% extract dimension of problem
P = length(theta_0);
N = length(y);

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

%  .frac_theta
try
    val = MCOPTS.frac_theta;
    if(or((val<=0),(val>=1)))
        error([routine_name, ...
                ': MCOPTS.frac_theta out of (0,1)']);
    end
catch
    MCOPTS.frac_theta = 0.5;
end
        
% .delta_theta
try
    val = MCOPTS.delta_theta;
catch
    MCOPTS.delta_theta = 0.1;
end

% .delta_sigma
try
    val = MCOPTS.delta_sigma;
catch
    MCOPTS.delta_sigma = 0.1*abs(sigma_0);
    if(MCOPTS.delta_sigma < sqrt(eps))
        MCOPTS.delta_sigma = sqrt(eps);
    end
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
% acceptance fractions and the numbers of samples
% taken.
num_theta_try = 0;
num_theta_accept = 0;
num_sigma_try = 0;
num_sigma_accept = 0;
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
sigma = sigma_0;
y_hat = feval(fun_yhat,theta,X_pred);
residuals = y - y_hat;
SSE = dot(residuals,residuals);
posterior_density = sigma^(-(N+1))* ...
    exp(-SSE/2/sigma^2);

% allocate space to store prediction of g
% and set to initially contain zeros
if(nargin >= 8)
    g_pred = feval(fun_g,theta,sigma,Param);
else
    g_pred = feval(fun_g,theta,sigma);
end
g_pred = zeros(size(g_pred));

% begin Monte Carlo iterations
delta_theta = MCOPTS.delta_theta;
delta_sigma = MCOPTS.delta_sigma;
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
    % do the same thing for the sigma moves
    if(num_sigma_try > 500);
        frac_accept = ...
            num_sigma_accept/num_sigma_try;
        if(frac_accept < MCOPTS.accept_tar)
            delta_sigma = delta_sigma * 0.9;
        elseif(frac_accept > MCOPTS.accept_tar)
            delta_sigma = delta_sigma * 1.1;
        end
        num_sigma_try = 0;
        num_sigma_accept = 0;
    end        
    
    % decide whether to do theta move or sigma
    % move
    u = rand;  % generate uniform random # on [0,1]
    if(u <= MCOPTS.frac_theta)  % do theta move
        do_theta_move = 1;
        do_sigma_move = 0;
    else
        do_theta_move = 0;
        do_sigma_move = 1;
    end
    if(do_theta_move)   % do theta move
        theta_new = theta + ...
            delta_theta.*avg_mag.* ...
            randn(size(theta));
        num_theta_try = num_theta_try + 1;
        % compute new predictions with this theta, and
        % the new weight according to the posterior
        % density
        y_hat_new = feval(fun_yhat,theta_new,X_pred);
        residuals_new = y - y_hat_new;
        SSE_new = dot(residuals_new,residuals_new);
        posterior_density_new = sigma^(-(N+1))* ...
            exp(-SSE_new/2/sigma^2);
        % compute the acceptance probability
        accept_prob = exp( (SSE - SSE_new) / (2*sigma^2) );
        if(accept_prob > 1)
            accept_prob = 1;
        end
        % decide whether to accept the move or not
        u = rand;
        if(u <= accept_prob)  % accept the move
            theta = theta_new;
            y_hat = y_hat_new;
            residuals = residuals_new;
            SSE = SSE_new;
            posterior_density = posterior_density_new;
            num_theta_accept = num_theta_accept + 1;
        end
    end
    if(do_sigma_move) % do sigma move
        sigma_new = sigma + delta_sigma*randn;
        num_sigma_try = num_sigma_try + 1;
        % compute the acceptance probability
        if(sigma_new <= 0)
            accept_prob = 0;
        else
            % compute new posterior density (but note that
            % the value of SSE does not change)
            posterior_density_new = -sigma_new^(-(N+1))* ...
                exp(-SSE/2/sigma_new^2);
            accept_prob = (sigma_new/sigma)^(-(N+1)) * ...
                exp(SSE/2*(-1/(sigma_new^2) + 1/(sigma^2)));
            if(accept_prob > 1)
                accept_prob = 1;
            end
        end
        % decide whether to accept the move
        u = rand;
        if(u <= accept_prob)
            sigma = sigma_new;
            posterior_density = posterior_density_new;
            num_sigma_accept = num_sigma_accept + 1;
        end
    end
    
    % if we are in the sampling stage, then measure the function
    % g(theta,sigma) and add to running sum.
    if(iter_MC > MCOPTS.N_equil)
        if(nargin >= 8)
            g_meas = feval(fun_g,theta,sigma,Param);
        else
            g_meas = feval(fun_g,theta,sigma);
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