function [beta] = logregFit2(y,X, lambda)
% logregFit2.m MLE for logistic regression
% Based on code by Mark Schmidt
%
% Rows of X contain data
% y(i) = 0 or 1
% 
% Returns beta, a row vector

[N p] = size(X);
beta = zeros(p,1);
options = optimset('Display','none','Diagnostics','off','GradObj','on','Hessian','on');
[beta, err] = fminunc(@logregGradient, beta, options, X, y, lambda);
