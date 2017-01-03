% MC    Monte Carlo valuation for a European call
%

%%%%%%%%%% Problem and method parameters %%%%%%%%%%%%
S = 2; E = 1; r = 0.05; sigma = 0.25; T = 3; M = 1e6;
randn('state',100)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Svals = S*exp((r-0.5*sigma^2)*T + sigma*sqrt(T)*randn(M,1));
Pvals = exp(-r*T)*max(Svals-E,0);
Pmean = mean(Pvals)
width = 1.96*std(Pvals)/sqrt(M); 
conf = [Pmean - width, Pmean + width]

