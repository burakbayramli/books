function out=lndetmc(order,iter,wsw,rmin,rmax)
% PURPOSE: computes Barry and Pace MC approximation to log det(I-rho*W)
% -----------------------------------------------------------------------
% USAGE: out = lndetmc(order,iter,W,rmin,rmax)
% where:      order = # of moments u'(wsw^j)u/(u'u) to examine (default = 50)
%              iter = how many realizations are employed (default = 30)
%                 W = symmetric spatial weight matrix (standardized)             
% -----------------------------------------------------------------------
% RETURNS: out = a structure variable
%          out.lndet = a vector of log determinants for -1 < rho < 1
%          out.rho   = a vector of rho values associated with lndet values
%          out.up95  = an upper 95% confidence interval on the approximation
%          out.lo95  = a lower  95% confidence interval on the approximation
% -----------------------------------------------------------------------
% NOTES: only produces results for a grid of 0 < rho < 1 by default
%        where the grid ranges by 0.01 increments
% -----------------------------------------------------------------------
% References: Ronald Barry and R. Kelley Pace, "A Monte Carlo Estimator
% of the Log Determinant of Large Sparse Matrices", Linear Algebra and
% its Applications", Volume 289, Number 1-3, 1999, pp. 41-54.
% -----------------------------------------------------------------------
 
 
% Written by Kelley Pace, 6/23/97 
% (named fmcdetnormgen1.m in the spatial statistics toolbox )
% Documentation modified by J. LeSage 11/99

if nargin == 3
rmin = 1e-5;
rmax = 1;
end;

[n,n]=size(wsw);

% Exact moments from 1 to oexact
td=full([0;sum(sum(wsw.^2))/2]);
oexact=length(td);

o=order;
% Stochastic moments

mavmomi=zeros(o,iter);
for j=1:iter;
u=randn(n,1);
v=u;
utu=u'*u;
for i=1:o;
v=wsw*v;
mavmomi(i,j)=n*((u'*v)/(i*utu));
end;
end;

mavmomi(1:oexact,:)=td(:,ones(iter,1));

%averages across iterations
avmomi=mean(mavmomi')';

clear u,v;

%alpha matrix

alpha=rmin:0.01:rmax;
valpha=vander(alpha);
valphaf=fliplr(valpha);
alomat=-valphaf(:,(2:(o+1)));

%Estimated ln|I-aD| using mixture of exact, stochastic moments
%exact from 1 to oexact, stochastic from (oexact+1) to o

lndetmat=alomat*avmomi;


%standard error computations
srvs=(alomat*mavmomi)';
sderr=(sqrt((mean(srvs.*srvs)-mean(srvs).^2)/iter))';

%lower bound computation
fbound=((n*alpha.^(o+1))./((o+1)*(1-alpha+eps)))';

%confidendence limits, with lower biased downward (more conservative)
low95=(lndetmat-1.96*sderr-fbound);
high95=(lndetmat+1.96*sderr);

%AR parameter, lower confidence limit, estimated log-det, upper confidence limit
% confide=[alpha'  low95 lndetmat high95];

out.rho = alpha';
out.lndet = lndetmat;
out.up95 = high95;
out.lo95 = low95;


