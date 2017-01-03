function y=sigmoid(x,beta)
%SIGMOID  1./(1+exp(-beta*x))
% y=sigmoid(x,beta) = 1./(1+exp(-beta*x))
y=1./(1+exp(-beta*x));