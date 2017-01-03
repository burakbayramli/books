function [samples, naccept] = metropolis(target, proposal, xinit, Nsamples, targetArgs, proposalArgs)
% Metropolis.m
% Metropolis algorithm (assumes a symmetric proposal distribution)
% function samples = metropolis(target, proposal, xinit, Nsamples, targetArgs, proposalArgs)
% 
% Inputs
% target is a function which will be called as 'p = target(x, targetArgs{:})'
% proposal is a function which will be called as 'xprime = proposal(x, proposalArgs{:})' where x is a 1xd vector
% xinit is a 1xd vector specifying the initial state
% Nsamples
%
% Outputs
% samples(t,:) is the t'th sample (of size d)
% naccept = number of accepted moves

if nargin < 5,  targetArgs = {}; end
if nargin < 6,  proposalArgs = {}; end


d = length(xinit);
samples = zeros(Nsamples, d);
x = xinit(:)';
naccept = 0;
for t=1:Nsamples
  xprime = feval(proposal, x, proposalArgs{:});
  alpha = min(1, feval(target, xprime, targetArgs{:})/feval(target, x, targetArgs{:}));
  u = rand(1,1);
  if u < alpha
    x = xprime;
    naccept = naccept + 1;
  end
  samples(t,:) = x;
end
  
