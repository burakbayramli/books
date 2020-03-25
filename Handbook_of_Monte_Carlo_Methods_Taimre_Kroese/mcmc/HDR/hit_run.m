function x=hit_run(x,gam)
% hit-and-run sampler
n=length(x);
d=randn(1,n); d=d/norm(d); % sample direction
lam=-x*d'+randn;
y=x+lam*d; % make proposal
if S(y)>gam
    x=y;
end






 























