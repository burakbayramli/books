%
%
% Gamma random variate generator
% Simon Rogers, 30/01/2007
% --------------------------------------------
% function g = mygamrnd(k,theta,N,varargin)
% generates N random variates from a Gamma(k,theta) pdf
% defined as
% p(g|k,theta) = g^{k-1} \frac{e^{-g/theta}}{\theta^k \Gamma(k)}
%
% Uses an acceptance-rejection method as described at
% http://en.wikipedia.org/wiki/Gamma_distribution
%
% varargin holds extra options, currently limited to
% 'verbose' {0,1} - 0 is silent (default), 1 prints some diagnostics
% e.g. g = mygamrnd(1,1,10,'verbose',1)
% generates 10 variates from a Gamma(1,1) distribution and displays info
%
%
function g = mygamrnd(alpha,theta,N,varargin)

verbose = 0;
for i = 1:2:length(varargin)-1
    switch lower(varargin{i})
        case 'verbose'
            verbose = varargin{i+1};
    end
end

k = floor(alpha);
delta = alpha-k;
g = zeros(N,1);
left = repmat(1,N,1);
if verbose
    fprintf('\n\nGamma Variate Generator');
    fprintf('\nSee http://en.wikipedia.org/wiki/Gamma_distribution');
    fprintf('\n[k]=%g, delta=%g, theta=%g, N=%g',k,delta,theta,N);
end
it = 0;
if delta>0
    nu = exp(1)/(exp(1)+delta);
    for n = 1:N
        while 1
            it = it + 1;
            V = rand(1,2);
            if V(1)<=nu
                xi = (V(1)/nu)^(1/delta);
                eta = V(2)*xi^(delta-1);
            else
                xi = 1 - log((V(1)-nu)/(1-nu));
                eta = V(2)*exp(-xi);
            end
            if eta <= xi^(delta-1) * exp(-xi)
                break
            end        
        end
        g(n) = xi;
    end
end
un = rand(N,k);
g = (theta)*(g - sum(log(un),2));
if verbose
    fprintf('\nFinished, avg number of its: %g\n\n',it/N);
end