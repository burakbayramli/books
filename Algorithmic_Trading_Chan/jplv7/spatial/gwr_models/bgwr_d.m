% PURPOSE: An example of using bgwr()
%          Geographically weighted regression model
%          (on a small data set)                  
%---------------------------------------------------
% USAGE: bgwr_d
%---------------------------------------------------

clear all;
% load the Anselin data set
load anselin.dat; y = anselin(:,1); nobs = length(y);
x = [ones(nobs,1) anselin(:,2:3)]; [junk nvar] = size(x);
east = anselin(:,4); north = anselin(:,5);
vnames = strvcat('crime','constant','income','hvalue');
ndraw = 550; nomit = 50;

[j1 W j2] = xy2cont(east,north);

info.dtype = 'exponential';
res1 = gwr(y,x,east,north,info);
prior.ctr = 21;
% these apply to all of the runs below
prior.dscale = 1e+6;
prior.dtype = info.dtype;
prior.rval = 30;
prior.bwidth = res1.bwidth;

prior.ptype = 'concentric';
bres1 = bgwr(y,x,east,north,ndraw,nomit,prior);

prior.ptype = 'distance';
bres2 = bgwr(y,x,east,north,ndraw,nomit,prior);

prior.ptype = 'contiguity';
prior.W = W;
bres3 = bgwr(y,x,east,north,ndraw,nomit,prior);

% compute posterior probabilities for 3 models
psum = bres1.lpost+bres2.lpost+bres3.lpost;
prob1 = bres1.lpost./psum;
prob2 = bres2.lpost./psum;
prob3 = bres3.lpost./psum;


tt=1:nobs;
plot(tt,prob1,'-',tt,prob2,'--',tt,prob3,'-.');
title('log posterior for 3 models');
legend('concentric','distance','contiguity');
pause;

tmp = mean(bres1.bdraw);
bout = squeeze(tmp);
[n k] = size(bout);
beta1 = zeros(n,k);
for i=1:k
beta1(:,i) = bout(:,i);
end;

tmp = mean(bres2.bdraw);
bout = squeeze(tmp);
[n k] = size(bout);
beta2 = zeros(n,k);
for i=1:k
beta2(:,i) = bout(:,i);
end;

tmp = mean(bres3.bdraw);
bout = squeeze(tmp);
[n k] = size(bout);
beta3 = zeros(n,k);
for i=1:k
beta3(:,i) = bout(:,i);
end;

% plot estimates for comparison
tt=1:n;
subplot(3,1,1), 
plot(tt,res1.beta(:,1),'-',tt,beta1(:,1),'--', ...
tt,beta2(:,1),'-.',tt,beta3(:,1),'o');
legend('gwr','concentric','distance','contiguity');
title('exponential weighting'); 
xlabel('constant');
subplot(3,1,2), 
plot(tt,res1.beta(:,2),'-',tt,beta1(:,2),'--', ...
tt,beta2(:,2),'-.',tt,beta3(:,2),'o'); 
xlabel('income');
subplot(3,1,3), 
plot(tt,res1.beta(:,3),'-',tt,beta1(:,3),'--', ...
tt,beta2(:,3),'-.',tt,beta3(:,3),'o'); 
xlabel('house value');
pause;


subplot(1,1,1),
plot(tt,bres1.vmean,tt,bres2.vmean,'--',tt,bres3.vmean,'-.');
title('vi estimates');
legend('concentric,','distance','contiguity');
pause;

subplot(1,1,1),
plot(tt,res1.sige,'o',tt,bres1.smean,tt,bres2.smean,'--',tt,bres3.smean,'-.');
title('sige estimates');
legend('gwr','concentric','distance','contiguity');


