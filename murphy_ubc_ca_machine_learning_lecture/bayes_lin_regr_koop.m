function bayes_lin_regr_koop()
% Conjugate bayesian analysis of linear regression
% from chapter 3 of Gary Koop, "Bayesian econometrics".
% Based on his code.

load hprice.txt; n=size(hprice,1); y=hprice(:,1); x=hprice(:,2:5); x=[ones(n,1) x];  k=5;

% Hyper-parameters
b0 = [0, 10, 5000, 10000, 10000]';
V0 = diag([2.4 6e-7 0.15 0.6 0.6]);
s02 = 1/4e-8; % sigma_0^{-2}
v0 = 5;

[bN, VN, sN2, vN, bsd, bhpdi95, probpos, lmarglik] = ...
    computePost(b0, V0, s02, v0, x, y);
probposun = probpos; % probability positive unrestricted model
lmargun = lmarglik; % log marginal likelihood unrestricted model

xstar = [1 5000 2 2 1];
[ystarm, ystarsd] = predict(xstar, bN, VN, sN2, vN);
fprintf('predicted price %5.3f +- %5.3f\n', ystarm, ystarsd);
fprintf('posterior\n');
for i=1:k
  fprintf('%3d %10.2f +- %10.2f  (in %10.1f to %10.1f wp 0.95)\n', ...
	  i, bN(i), bsd(i), bhpdi95(i,1), bhpdi95(i,2));
end

% Repeat analysis with uninformative prior
v0=0;
V0 = zeros(k);
[bN, VN, sN2, vN, bsd, bhpdi95, probpos] = ...
    computePost(b0, V0, s02, v0, x, y);

fprintf('with uninformative prior\n');
[ystarm, ystarsd] = predict(xstar, bN, VN, sN2, vN);
fprintf('predicted price %5.3f +- %5.3f\n', ystarm, ystarsd);
fprintf('posterior\n');
for i=1:k
  fprintf('%3d %10.2f +- %10.2f  (in %10.1f to %10.1f wp 0.95)\n', ...
	  i, bN(i), bsd(i), bhpdi95(i,1), bhpdi95(i,2));
end


% Model selection

% Go back to informative prior
V0 = diag([2.4 6e-7 0.15 0.6 0.6]);
v0 = 5;

for i=1:k
  % consider omitting component i
  incl = [1:i-1 i+1:k]; % dimensions to include
  [bN, VN, sN2, vN, bsd, bhpdi95, probpos, lmarglik] = ...
    computePost(b0(incl), V0(incl,incl), s02, v0, x(:,incl), y);
  postodds(i)=exp(lmarglik-lmargun);
end

modelprob = postodds./(1+postodds);

fprintf('model assesment\n');
for i=1:k
  fprintf('%d: p(w_j>0|D)=%5.3f, P(w_j=0|D)/P(w_j != 0|D)=%5.3f, P(w_j=0|D)=%5.3f\n', ...
	  i, probposun(i), postodds(i), modelprob(i));
end

% Monte carlo integration
samples = [10 100 1000 10000];
%samples = [1000];
clear bs bsd nse
for i=1:length(samples)
  S = samples(i);
  [bs(:,i), bsd(:,i), nse(:,i)] = computePostMC(b0, V0, s02, v0, x, y, S, @extractMarginal);
end

fprintf('MC\n');
marg = [1 2];
for mi=1:length(marg)
  m=marg(mi);
  for i=1:length(samples)
    S = samples(i);
    fprintf('S=%6d, E[b(%1d)]=%10.5f +- %10.5f, nse = %10.5f\n', ...
	    S, m, bs(mi, i), bsd(mi, i), nse(mi, i));
  end
end


%%%%%%%%%%

function m = extractMarginal(b)
m = b(1:2);

%%%%%%%%%%

function [fmean, fsd, nse] = computePostMC(b0, V0, s02, v0, x, y, nsamples, fn);
% Compute E[fn(b)], sd[fn(b)] using nsamples of monte carlo

[b1, capv1, s12, v1] =  computePost(b0, V0, s02, v0, x, y);
k = size(x,2);

vscale = s12*capv1;
vchol=chol(vscale);
vchol=vchol';

fmean = zeros(size(feval(fn, b1)));
fsq = zeros(size(fmean));

for i=1:nsamples
  bsamp = b1 + vchol*trnd(v1, k, 1); % draw from T(b1, vscale, v1)
  f = feval(fn, bsamp);
  fmean = fmean + f;
  fsq = fsq + f.^2;
end
fmean = fmean / nsamples;
fsq = fsq / nsamples;
fvar = fsq - fmean.^2;
fsd = sqrt(fvar);
nse = fsd ./ sqrt(nsamples); % numerical standard error


%%%%%%%%%%

function [b1, capv1, s12, v1, bsd, bhpdi95, probpos, lmarglik] = ...
    computePost(b0, V0, s02, v0, x, y);
% bsd(i) = std dev of P( beta(i) | D)
% bhpdi(i,1:2) = 95% high prob density interval
% probpos(i) = P(beta(i) > 0 | D)

[n k] = size(x);
capv0  = V0;
if det(V0)>0
  capv0inv = inv(V0);
else
  capv0inv = zeros(k);
end

bols = inv(x'*x)*x'*y; %Ordinary least squares quantities
s2 = (y-x*bols)'*(y-x*bols)/(n-k);
bolscov = s2*inv(x'*x);
bolssd=zeros(k,1);
for i = 1:k
  bolssd(i,1)=sqrt(bolscov(i,i));
end
v=n-k;

xsquare=x'*x;
v1=v0+n;
capv1inv = capv0inv+ xsquare;
capv1=inv(capv1inv);
b1 = capv1*(capv0inv*b0 + xsquare*bols);
if det(capv0inv)>0
  v1s12 = v0*s02 + v*s2 + (bols-b0)'*inv(capv0 + inv(xsquare))*(bols-b0);
else
  v1s12 = v0*s02 + v*s2;
end
s12 = v1s12/v1;

bcov = capv1*v1s12/(v1-2);
bsd=zeros(k,1);
for i = 1:k
  bsd(i,1)=sqrt(bcov(i,i));
end

probpos=zeros(k,1);
bhpdi95=zeros(k,2);
invcdf95=tinv(.975,v1);
for i = 1:k
  tnorm = -b1(i,1)/sqrt(s12*capv1(i,i));
  probpos(i,1) = 1 - tcdf(tnorm,v1);
  bhpdi95(i,1) = b1(i,1)-invcdf95*sqrt(s12*capv1(i,i));
  bhpdi95(i,2) = b1(i,1)+invcdf95*sqrt(s12*capv1(i,i));
end

%log of marginal likelihood for the model if prior is informative
if det(capv0inv)>0;
  intcon=gammaln(.5*v1) + .5*v0*log(v0*s02)- gammaln(.5*v0) -.5*n*log(pi);
  lmarglik=intcon + .5*log(det(capv1)/det(capv0)) - .5*v1*log(v1s12);
end

%%%%%%%%
function [ystarm, ystarsd] = predict(xstar, b1, capv1, s12, v1)
ystarm = xstar*b1;
ystarcapv = (1+ xstar*capv1*xstar')*s12;
ystarv = ystarcapv*v1/(v1-2);
ystarsd=sqrt(ystarv);

