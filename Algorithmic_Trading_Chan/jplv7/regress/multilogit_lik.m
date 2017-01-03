function [P,lnL] = multilogit_lik(y,x,beta,d);
% PURPOSE: Computes value of log likelihood function for multinomial logit regression

% written by:
% Simon D. Woodcock
% CISER / Economics
% Cornell University
% Ithaca, NY 
% sdw9@cornell.edu

[nvar ncat] = size(beta);
[nobs junk] = size(x);
xb = x*beta;
e_xb = exp(xb);
sum_e_xb = sum(e_xb');
for j = 1:ncat
    P(:,j) = e_xb(:,j) ./ (1 + sum_e_xb');
end;
P_0 = ones(nobs,1) - sum(P')';
p = [P_0 P];
d_0 = (y == min(y));
d = [d_0 d];
lnp = log(p);
contribution = d .* lnp;
c_0 = sum(y == 0);
lnL = sum(sum(contribution'));

