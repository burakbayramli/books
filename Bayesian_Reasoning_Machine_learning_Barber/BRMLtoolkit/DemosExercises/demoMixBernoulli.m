function demoMixBernoulli
%DEMOMIXBERNOULLI demo of training a mixture of Bernoulli distributions
H = 2; % number of hidden states
ph_true = condp(1:H); % hidden distribution
ph_true = condp([0.3 0.7]');
N = 150; % number of training examples
D = 5;  % dimension of each training example
pvgh_true =  randgen([0.5 0.5],D,H,[0.2 0.8]);
pvgh_true=pvgh_true.*(1+0.095*rand(D,H)); % make so closer to extreme probabilities
% sample some data from this distribution:
max_miss = 2; % maximum number of missing values in each record
for n = 1:N
	h_true(n)=randgen(ph_true); % first get the hidden state
	v(:,n) = real(rand(D,1) < pvgh_true(:,h_true(n))); % generate data from p(v|h)
	% simulate missing data : missing up to max_miss values
	r = randperm(D); miss = floor(rand*(max_miss+1)); % number of missing entries
	v(r(1:miss),n) = 0.5*ones(miss,1); % code missing data as -1
end
%[val,ind]=sort(h_true);
%v=v(:,ind); h_true=h_true(ind); % sort to ease interpretation later

figure
% EM training:
opts.plotprogress=1; opts.maxit=100;
% Perform multiple runs (due to local maxima issues:)
loglik=-inf;
for runs=1:3
	[phr pvghr thisloglik phgvr]=MIXprodBern(v,H,opts);
	if thisloglik>loglik
		ph=phr; pvgh=pvghr; phgv=phgvr; loglik=thisloglik
	end
end
figure; imagesc(v); colormap('gray');  axis([0 N+1 1 D]); title('training data');
figure; subplot(1,2,1); bar(ph_true); axis([1 H 0 1]);  colormap('gray'); title('p(h) true ')
 
[val,ord] = sort(ph); subplot(1,2,2); bar(ph(ord));  axis([1 H 0 1]);  title('p(h) estimated')

figure;subplot(1,2,1); bar(pvgh_true); axis([0 D+1 0 1]); title('p(v|h) true')

subplot(1,2,2); bar(pvgh(:,ord));axis([0 D+1 0 1]); colormap('gray');title('p(v|h) estimated')

figure
b=bar(1-phgvr(1,:)); set(b,'facecolor',[0.8 0.8 0.8]); axis([0 N+1 0 1]); title('p(h|v)')
figure
b=bar(h_true-1); set(b,'facecolor',[0.8 0.8 0.8]); axis([0 N+1 0 1]); title('true h')
