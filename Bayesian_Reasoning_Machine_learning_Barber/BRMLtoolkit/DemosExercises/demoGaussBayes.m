function demoGaussBayes
%DEMOGAUSSBAYES demo of Bayesian inference of Gaussian mean and variance
true_ss=1; true_mean=2; N=10; % true Gaussian parameters
x=mvrandn(true_mean,true_ss,N); % sample some data from the Gaussian

mu0 = 0;  alpha=2; beta=1; gamma=1; % prior Gauss-Gamma parameters 

a_tilde = N+1/gamma; b_tilde = mu0/gamma+sum(x); c_tilde = mu0^2/gamma+sum(x.^2);

%sigma2_hat = cov([mu0 x],1); m_hat = mean([mu0 x]);

lambda = 0:0.1:4; mu=-5:0.1:5; % precision and mean ranges of interest in plot


logpdf_prior= logGaussGamma(mu,lambda,mu0,alpha,beta,gamma); %unnormalised prior
surf(mu,lambda,exp(logpdf_prior'-max(max(logpdf_prior'))))
xlabel('mean'); ylabel('precision');

beta_tilde=1/(1/beta+0.5*(c_tilde-b_tilde^2/a_tilde)); alpha_tilde = alpha+N/2; % posterior Gauss-Gamma parameters
gamma_tilde=1/a_tilde; mu0_tilde = b_tilde/a_tilde;
figure
logpdf_post = logGaussGamma(mu,lambda,mu0_tilde,alpha_tilde,beta_tilde,gamma_tilde);
surf(mu,lambda,exp(logpdf_post'-max(max(logpdf_post'))))
xlabel('mean'); ylabel('precision');