function logpdf = logGaussGamma(mu,lambda,mn,alpha,beta,gamma)
%LOGGAUSSGAMMA unnormalised log of the Gauss-Gamma distribution
l=0;
for lambda_val = lambda
    l=l+1;   m=0; 
    v=gamma/lambda_val; % variance of the Gaussian
    for mu_val = mu
        m=m+1;
        logpdf(m,l) = -0.5*(mu_val - mn)^2./v -0.5*log(v)... % Gaussian part
            +(alpha-1)*log(lambda_val)-lambda_val/beta; % Gamma part,  neglecting constants in both
    end
end