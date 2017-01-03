function demoHMMinferenceSimple
%DEMOHMMINFERENCESIMPLE another HMM inference demo
H = 6; % number of Hidden states
V = 3; % number of Visible states
T = 10; % length of the time-series
% setup the HMM
phghm = condp(rand(H,H)+3*eye(H));% transition distribution p(h(t)|h(t-1))
pvgh = condp(rand(V,H).^5);% emission distribution p(v(t)|h(t))
ph1 = condp(ones(H,1)); % initial p(h)
% generate some fake data

h(1) = randgen(ph1); v(1)=randgen(pvgh(:,h(1)));
for t=2:T
	h(t)=randgen(phghm(:,h(t-1)));	v(t)=randgen(pvgh(:,h(t)));
end
h
v
% Perform Inference tasks:
[logalpha,loglik]=HMMforward(v,phghm,ph1,pvgh); % forward
% smoothed posteriors:
gamma=HMMgamma(logalpha,phghm) % alternative alpha-gamma (RTS) method
[viterbimaxstate logprob]=HMMviterbi(v,phghm,ph1,pvgh) % most likely joint state