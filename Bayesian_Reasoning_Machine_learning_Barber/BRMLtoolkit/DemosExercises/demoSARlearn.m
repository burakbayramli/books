function demoSARlearn
%DEMOSARLEARN demo of learning in a Switching Autoregressive Model
S = 3; % number of Hidden states
L = 2; % order of each AR model
T = 100; % length of the time-series
Tskip=10;

x=1:T;
for s=1:S
	% generate a sequence and get the AR coeffs by fitting the sequence:
	y=sin(1*x*rand)+0.01*randn(1,T); a_true(:,s) = ARtrain(y,L);
end
stran_true = condp(rand(S,S)+1*eye(S)); % switch transition
sprior_true=condp(ones(S,1)); % switch prior
sigma2_true=0.01*ones(1,S);

% generate some training data:
v=randn(1,T); % random initial visible variable
s_true(1)=randgen(sprior_true); % random initial switch state
for t=2:T
	Lt = min(t-1,L); % to handle the start when not enough timepoints exist
	vhat = v(t-Lt:t-1)';
	if mod(t,Tskip)==0
		s_true(t) = randgen(stran_true(:,s_true(t-1)));
	else
		s_true(t)=s_true(t-1);
	end
	v(t)=a_true(1:Lt,s_true(t))'*vhat+sqrt(sigma2_true(s_true(t)))*randn;
end
st=zeros(S,T);for t=1:T; st(s_true(t),t)=1; end % (for plotting later)
figure(1); subplot(2,1,1); plot(v,'k');  hold on; c=hsv(S);
title('sample switches')
for s=1:S
	tt=find(s_true==s);
	plot(tt,v(tt),'.','color',c(s,:));
end

% EM training:
figure
opts.plotprogress=1;
opts.maxit=30;
opts.stran=stran_true;
opts.sigma2=sigma2_true;
[a,sigma2,stran,phtgV1T]=SARlearn(v,L,S,Tskip,opts);

[val ind]=max(phtgV1T); % find the most likely MPM switches
figure(1);
subplot(2,1,2); plot(v,'k'); hold on; title('learned switches')
for s=1:S
	tt=find(ind==s);
	plot(tt,v(tt),'.','color',c(s,:));
end