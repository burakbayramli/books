function pr_sm0 = ksmooth(parm,pr_tt0,pr_tl0)
% PURPOSE: Kim's smoothing for Hamilton() model
% ---------------------------------------------------
% USAGE: [prob1 prob2] = ksmooth(parm,prob1,prob2)
% where: parm = maximum likelihood estimates
%       prob1 = filtered probabilities from hfilter()
%               pr(S_t | y_t)
%       prob2 = filtered probabilities from hfilter()
%               pr(S_t | y_t-1)
% ----------------------------------------------------
% % RETURNS: prob1 = Pr[S_t=0|Y_t], a vector of smoothed probabilities
% ---------------------------------------------------------

% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com

if nargin ~= 3
error('ksmooth: Wrong # of input arguments');
end;

n = length(pr_tt0);

ppr = parm(1,1);
qpr = parm(2,1);

pr_tt1 = ones(n,1)-pr_tt0;
pr_tl1 = ones(n,1)-pr_tl0;

pr_sm0 = pr_tt0;
pr_sm1 = pr_tt1;

iter = n-1;
while iter >= 1

% p(S_t, S_t+1 | y_t)
pr_sm00 = pr_sm0(iter+1,1)*qpr*pr_tt0(iter,1);
pr_sm00 = pr_sm00/pr_tl0(iter+1,1);
pr_sm01 = pr_sm1(iter+1,1)*(1-qpr)*pr_tt0(iter,1);
pr_sm01 = pr_sm01/pr_tl1(iter+1,1);
pr_sm10 = pr_sm0(iter+1,1)*(1-ppr)*pr_tt1(iter,1);
pr_sm10 = pr_sm10/pr_tl0(iter+1,1);
pr_sm11 = pr_sm1(iter+1,1)*ppr*pr_tt1(iter,1);
pr_sm11 = pr_sm11/pr_tl1(iter+1,1);

pr_sm0(iter,1) = pr_sm00 + pr_sm01;
pr_sm1(iter,1) = pr_sm10 + pr_sm11;

iter = iter - 1;
end;

