function [phghm,ph1,pvgh,loglikelihood]=HMMem(v,H,V,opts)
%HMMEM EM algorithm for HMM
% [phghm,ph1,pvgh,loglikelihood]=HMMem(v,H,V,opts)
%
% Inputs:
% v : cell array containing sequences, so v{3}(5) is the 3rd sequence, 5th timestep
% H : number of hidden states
% V : number of visible (observation) states 
% opts.maxit : maximum iterations
% opts.plotprogress : set to 1 to plot the loglikelihood
%
% Outputs:
% phghm : learned transition p(h(t)|h(t-1))
% ph1  : learned initial distribution p(h(1))
% pvgh : learned emission p(v|h) 
% loglikelihood : log likelihood of the sequences
N = length(v); % number of sequences
% random initialisation:
phghm= condp(rand(H,H)); % transition distribution p(h(t)|h(t-1))
pvgh= condp(rand(V,H)); % emission distribution p(v(t)|h(t))
ph1= condp(rand(H,1)); % initial p(h)

for emloop=1:opts.maxit
	A=zeros(H,H); a=zeros(H,1); B=zeros(V,H);
	llik(emloop)=0;
	for n=1:N
		% Perform Inference tasks (E step):
		[logalpha,loglik]=HMMforward(v{n},phghm,ph1,pvgh); % forward
		llik(emloop)=llik(emloop)+loglik;
		logbeta=HMMbackward(v{n},phghm,pvgh); % backward
		[phtgV1T,phthtpgV1T]=HMMsmooth(logalpha,logbeta,pvgh,phghm,v{n});
		% collect the statistics for the M-step:
		A = A + sum(phthtpgV1T,3);
		a = a + phtgV1T(:,1);
		for t=1:length(v{n})
			B(v{n}(t),:)=B(v{n}(t),:)+phtgV1T(:,t)';
		end
	end
	ph1=condp(a); % M-step
	phghm=condp(A');
	pvgh=condp(B);
	if opts.plotprogress; plot(llik); title('log likelihood'); drawnow; end
end
loglikelihood=loglik(end);