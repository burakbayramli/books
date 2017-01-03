function [ph,pv1gh,pvgvh,loglikelihood,phgv]=mixMarkov(v,V,H,opts)
%MIXMARKOV EM training for a mixture of Markov Models
%[ph,pv1gh,pvgvh,loglikelihood,phgv]=mixMarkov(v,V,H,opts)
%
% Inputs:
% v : cell array of the data. v{1} contains the first data sequence, v{2} the next etc.
% V : the number of visible states of the data
% H : number of mixture componetns.
% opts.plotprogress=1 to display log likelihood
% opts.maxit : the number of iterations of the EM algorithm
%
% Outputs:
% ph : learned p(h)
% pv1gh : p(v(1)|h)
% pvgvh : p(v(t)|v(t-1),h)
% loglikelihood : log likelihood of all the sequences (assuming iid)
% phgv : posterior probability p(h|v) for each sequence
% See also demoMixMarkov.m
ph= condp(rand(H,1)); % random parameter initialisation
pv1gh=condp(rand(V,H));
pvgvh=condp(rand(V,V,H));

for emloop=1:opts.maxit
	% E-step
	ph_stat=zeros(H,1);
	pv1gh_stat =zeros(V,H);
	pvgvh_stat =zeros(V,V,H);
	loglik=0;
	for n=1:length(v)
		T = length(v{n});
		lph_old =log(ph)+log(pv1gh(v{n}(1),:)');
		for t=2:T
			lph_old=lph_old+squeeze(log(pvgvh(v{n}(t),v{n}(t-1),:)));
		end
		ph_old{n}=condexp(lph_old);
		loglik = loglik + logsumexp(lph_old,ones(H,1)); % avoids numerical underflow
		% collect statistics for M-step:
		ph_stat=ph_stat + ph_old{n};
		pv1gh_stat(v{n}(1),:)=pv1gh_stat(v{n}(1),:) + ph_old{n}';
		for t=2:T
			pvgvh_stat(v{n}(t),v{n}(t-1),:) = squeeze(pvgvh_stat(v{n}(t),v{n}(t-1),:))+ ph_old{n};
		end
	end
	llik(emloop)=loglik;
	if opts.plotprogress; plot(llik); title('log likelihood'); drawnow; end
	% M-step
	ph = condp(ph_stat);
	pv1gh = condp(pv1gh_stat);
	pvgvh = condp(pvgvh_stat);
end
loglikelihood=llik(end); phgv=ph_old;