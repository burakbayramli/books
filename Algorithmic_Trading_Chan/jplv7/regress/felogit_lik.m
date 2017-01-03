function [P,lnL] = felogit_lik(y,x,b,c,d)
% PURPOSE: Compute probabilities and value of log-likelihood
%   for fixed effects logit model. Called from felogit()

% written by:
% Simon D. Woodcock
% CISER / Economics
% Cornell University
% 201 Caldwell Hall
% Ithaca, NY 14850
% sdw9@cornell.edu

index = x*b + d*c;
e_index = exp(index);
P = e_index ./ ( 1 + e_index);
lnL = sum( y.*index - log(1+e_index) );

