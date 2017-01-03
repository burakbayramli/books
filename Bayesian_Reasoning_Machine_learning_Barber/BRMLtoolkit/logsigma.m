function l=logsigma(x)
%LOGSIGMA log(1./(1+exp(-x)))
if x<0
    l = x - log(1+exp(x));
else
    l = -log(1+exp(-x));
end