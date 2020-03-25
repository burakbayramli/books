%peaks\simplepeaks_noisy.m
n = 2;                              % dimension
mu = [-3,-3]; sigma = 3*ones(1,n); N = 100; eps = 1E-5; rho=0.1;
while max(sigma) > eps
   X = randn(N,n)*diag(sigma)+ mu(ones(N,1),:);
   SX= S(X);                        %Compute the performance
   SX= SX+randn(N,1);               %Corrupt with noise
   sortSX = sortrows([X, SX],n+1);
   Elite = sortSX((1-rho)*N:N,1:n); % elite samples
   mu = mean(Elite,1);              % take sample mean row-wise
   sigma = std(Elite,1);            % take sample st.dev. row-wise
   [S(mu)+randn,mu,max(sigma)]            % output the result
end
