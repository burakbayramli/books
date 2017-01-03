%TSQMC.M           Mean exit time via Monte Carlo
%                  for mean-reverting sqrt process.
%
%                  Call tsqexact to evaluate ``exact'' answer via the BVP.
%

randn('state',100)  % set the state of randn

lambda = 1;         % problem parameters
mu = 0.5;           %
sigma = 0.3;        %
a = 1;              %
b = 2;              %
Xzero = 1.5;        % initial condition

%%%%%%%%%%%%%% Monte Carlo %%%%%%%%%%%%%%%%%%%%%
Dt = 1e-3;  % stepsize 
M = 1e3;    % number of paths

texit = zeros(M,1);
for s = 1:M
     s
     X = Xzero;
     t = 0;
     while X > a & X < b,
         dW = sqrt(Dt)*randn;            % Brownian increment
         X = X + Dt*lambda*(mu-X) + dW*sigma*sqrt(abs(X));
         t = t + Dt;
     end
     texit(s) = t - 0.5*Dt;
end

tmean = mean(texit)
tstd = std(texit);
cileft = tmean - 1.96*tstd/sqrt(M);
ciright = tmean + 1.96*tstd/sqrt(M);
conf = [cileft, ciright]

texact = tsqexact(Xzero,a,b,lambda,mu,sigma)

