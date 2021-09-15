% calc_MRSL_posterior.m
%
% [F_cost, det_S, posterior_density] = ...
%     calc_MRSL_posterior(theta,MRSLData,det_S_ref);
%
% This routine computes the multi-reponse
% sequential learning marginal posterior
% density for theta, a set of parameters,
% from a composite data set of several
% different multi-response data matrices.
%
% The inputs are:
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
% det_S_ref: a vector of values of det(S) for some reference
%      value of theta. This is used to avoid problems with
%      numerical overflow of the posterior density.
%
% The outputs are:
% F_cost: the cost function value for theta, where the most
%         probable estimate is the global minimum of F_cost
% det_S_vals: the vector of det(S) values in each data set
% posterior_density: the posterior density generated from the
%         composite data set.
%
% K.J. Beers. MIT ChE. 12/16/2004. ver. 12/17/2004

function [F_cost, det_S, posterior_density] = ...
    calc_MRSL_posterior(theta, MRSLData, det_S_ref);

P = length(theta);

% consider each data set in turn, and compute
% the contributions to the cost function and the
% posterior density.
F_cost = 0;
if(nargout >= 2)
    det_S = zeros(MRSLData.num_sets,1);
end
if(nargout >= 3)
    posterior_density = 1;
end
% iterate over each data set
for id=1:MRSLData.num_sets

    % extract the predict and measured response
    % data matrices
    X_pred_name = ['MRSLData.X_pred_', int2str(id)];
    X_pred = eval(X_pred_name);
    Y_name = ['MRSLData.Y_', int2str(id)];
    Y = eval(Y_name);
    N = size(Y,1);
    M = size(X_pred,2);
    L = size(Y,2);
    
    % compute the predicted response data
    fun_yhat = eval(['MRSLData.fun_yhat_', int2str(id)]);
    Y_hat = feval(fun_yhat, theta, X_pred);
    
    % compute the sum of squares matrix
    Delta = Y - Y_hat;
    S = Delta'*Delta;
    % account for the contribution to the data
    % set to the cost function and the posterior
    % density of the composite data set.
    det_S_val = abs(det(S));
    if(nargout >= 2)
        det_S(id) = det_S_val;
    end
    F_cost = F_cost + 0.5*N*log(det_S_val);
    if(nargout >= 3)
        if(nargin >= 3)
            det_S_ref_val = det_S_ref(id);
        else
            det_S_ref_val = d1;
        end
        posterior_density = posterior_density * ...
            (det_S_val/det_S_ref_val)^(-N/2);
    end
end


return;

