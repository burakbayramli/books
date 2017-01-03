% PURPOSE: An example of using sar_gmm on a simulated large data set   
%          GM estimation of the spatial autoregressive model                         
%---------------------------------------------------
% USAGE: sar_gmmd3 (see sar_gmmd for a small data set)
%---------------------------------------------------

clear all;
% NOTE a large data set with 3107 observations
% from Pace and Barry, takes around 150-250 seconds
load elect.dat;                    % load data on votes
latt = elect(:,5);
long = elect(:,6);
n = length(latt);
k = 4;
clear elect;                % conserve on RAM memory

[j,W,j] = xy2cont(latt,long); % contiguity-based spatial Weight matrix

rho = 0.7;
beta = ones(k,1);
B = speye(n) - rho*W;

x = randn(n,k);

sigvec = 1:10:100;

niter = length(sigvec);

gparms = zeros(niter,5);
mparms = zeros(niter,5);

for iter=1:niter;
    
    sige = sigvec(iter);

y = B\(x*beta) + B\(randn(n,1)*sqrt(sige));


% use defaults including lndet approximation
result = sar(y,x,W); % maximum likelihood estimates
mparms(iter,5) = result.rho;

p = result.p;
total = result.total;
total_out = zeros(p,1);
for i=1:p;
tmp = squeeze(total(:,i,:)); % an ndraw by 1 by ntraces matrix
tmpp = sum(tmp');
total_mean = mean(tmpp');
mparms(iter,i) = total_mean;
end;


result2 = sar_gmm(y,x,W);
gparms(iter,5) = result2.rho;
p = result2.p;
total = result2.total;
total_out = zeros(p,1);
for i=1:p;
tmp = squeeze(total(:,i,:)); % an ndraw by 1 by ntraces matrix
tmpp = sum(tmp');
total_mean = mean(tmpp');
gparms(iter,i) = total_mean;
end;

end;

tt=1:niter;

rhot = rho*ones(niter,1);

plot(sigvec,mparms(:,5),'+',sigvec,gparms(:,5),'o',sigvec,rhot,'-');
title('rho estimates');
legend('ML','GMM','truth');
xlabel('noise variance');
pause;

betat = 3.5*ones(niter,1);

for iter=1:4;
plot(sigvec,mparms(:,iter),'+',sigvec,gparms(:,iter),'o',sigvec,betat,'-');
title('total effects estimates');
legend('ML','GMM','truth');
xlabel('noise variance');
pause;
end;




