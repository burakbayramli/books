function [m S]=GaussCond(obs,mean_all,S_all)
%GAUSSCOND Return the mean and covariance of a conditioned Gaussian
% [m S]=GaussCond(obs,mean_all,S_all)
% Those elements of obs that are nan are the non-conditioned variables
obs=obs(:);
rem_var = find(isnan(obs)); rem_var=rem_var(:);
obs_var = find(~isnan(obs)); obs_var=obs_var(:);
m = mean_all(rem_var)+S_all(rem_var,obs_var)/(S_all(obs_var,obs_var))*(obs(obs_var)-mean_all(obs_var));
S = S_all(rem_var,rem_var)-S_all(rem_var,obs_var)/(S_all(obs_var,obs_var))*S_all(obs_var,rem_var);