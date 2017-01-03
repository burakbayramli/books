function demoHMMlearn
%DEMOHMMLEARN demo of learning the parameters of a HMM using EM
H = 3; % number of Hidden states
V = 2; % number of Visible states
N = 10; % number of sequences

% setup the true HMM
phghm_true = condp(rand(H,H)); % transition distribution p(h(t)|h(t-1))
pvgh_true = condp(rand(V,H)); % emission distribution p(v(t)|h(t))
ph1_true = condp(rand(H,1)); % initial p(h)

% generate some training data
for n=1:N
	T(n) = 20+ceil(10*rand); % length of the time-series
	h{n}(1) = randgen(ph1_true); v{n}(1)=randgen(pvgh_true(:,h{n}(1)));
	for t=2:T(n)
		h{n}(t)=randgen(phghm_true(:,h{n}(t-1))); v{n}(t)=randgen(pvgh_true(:,h{n}(t)));
	end
end

% EM algorithm (see if we can recover the true HMM parameters):
% random initialisation:
figure
opts.plotprogress =1; opts.maxit=10;
[phghm,ph1,pvgh,loglik]=HMMem(v,H,V,opts);

figure; [dum hord_true]=sort(ph1_true);[dum hord]=sort(ph1); % sort the hidden units to aid visualisation
subplot(1,2,1); bar(ph1_true(hord_true)); title('true initial p(h)');
subplot(1,2,2); bar(ph1(hord)); title('learned initial p(h)');
figure;
subplot(1,2,1); imagesc(phghm_true(hord_true,hord_true)); title('true transition');
subplot(1,2,2); imagesc(phghm(hord,hord)); title('learned transition');
figure;
subplot(1,2,1); imagesc(pvgh_true(:,hord_true)); title('true emission');
subplot(1,2,2); imagesc(pvgh(:,hord)); title('learned emission');