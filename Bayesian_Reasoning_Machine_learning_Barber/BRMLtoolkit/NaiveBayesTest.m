function px=NaiveBayesTest(x,pA,pB,mA,mB)
%NAIVEBAYESTEST Test Naive Bayes Bernoulli Distribution after Max Likelihood training
% px=NaiveBayesTest(x,pA,pB,mA,mB) 
%
% Inputs:
% x : data matrix contains elements 1,0
% pA : probability of class A
% pB : probability of class B
% mA : p(x=1|class A)
% mB : p(x=1|class B)
%
% Output : px -- [p(class A|x), p(class B|x)]
% See also NaiveBayesTrain.m and demoNaiveBayes.m
logpAandX = log(pA) + sum(log(mA(find(x==1))))+sum(log(1-mA(find(x==0))));
logpBandX = log(pB)+ sum(log(mB(find(x==1))))+sum(log(1-mB(find(x==0))));
px = condexp([logpAandX logpBandX]');