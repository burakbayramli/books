%STINT  Approximate stochastic integral
%
% Ito integral of W dW

randn('state',100)                      % set the state of randn
T = 1; N = 500; dt = T/N;

dW = sqrt(dt)*randn(1,N);               % increments
W = cumsum(dW);                         % cumulative sum

ito = sum([0,W(1:end-1)].*dW)    

itoerr = abs(ito - 0.5*(W(end)^2-T))


