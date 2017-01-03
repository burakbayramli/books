function [pA pB mA mB]=NaiveBayesTrain(xA,xB) 
%NAIVEBAYESTRAIN Train Naive Bayes Bernoulli Distribution using Max Likelihood
% [pA pB mA mB]=NaiveBayesTrain(xA,xB) 
%
% Inputs:
% xA : data matrix for class A -- contains elements 1,0
% xB : data matrix for class B -- contains elements 1,0
% 
% Outputs:
% pA : probability of class A
% pB : probability of class B
% mA : p(x=1|class A)
% mB : p(x=1|class B)
pA= size(xA,2)/(size(xA,2) + size(xB,2));
pB=1-pA; % ML priors 
mA= mynanmean(xA,2); % ML estimates of p(x=1|A)
mB= mynanmean(xB,2); % ML estimates of p(x=1|B)