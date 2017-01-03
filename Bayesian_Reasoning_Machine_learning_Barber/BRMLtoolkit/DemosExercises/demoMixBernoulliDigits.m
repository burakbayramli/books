function demoMixBernoulliDigits
%DEMOMIXBERNOULLI demo of training a mixture of Bernoulli distributions
v=[];
for d=0:9
    load(['digit',num2str(d)'])
    xx=x';
    v=[v xx(:,1:500)];
end
v=v>0;

figure; disp('plotting subset of training data')
r=randperm(size(v,2));
v=v(:,r);
for h=1:40*5
    subplot(5,40,h,'align'); 
    imagesc(reshape(v(:,h),28,28)'); axis off; colormap bone
end


figure
% EM training:
H=10;
opts.plotprogress=1; opts.maxit=25;
opts.meaninit=1;
% Perform multiple runs (due to local maxima issues:)
loglik=-inf;
for runs=1:1
	[phr pvghr thisloglik phgvr]=MIXprodBern(v,H,opts);
	if thisloglik>loglik
		ph=phr; pvgh=pvghr; phgv=phgvr; loglik=thisloglik
	end
end
figure
for h=1:H
    subplot(1,H,h,'align'); 
    imagesc(reshape(pvgh(:,h),28,28)'); axis off;% colormap bone
end
disp('plotted Mixture components')


