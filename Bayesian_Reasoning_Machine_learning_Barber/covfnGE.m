function K = covfnGE(x,y,par)
%COVFNGE Gamma Exponential Covariance Function
%K = covfnGE(x,y,par)
K = par(1).value.*exp(-par(2).value*(sqdist(x,y)).^(0.5*par(3).value));
if length(par)==4
    K = K + par(4).value*eye(size(K,1));
end