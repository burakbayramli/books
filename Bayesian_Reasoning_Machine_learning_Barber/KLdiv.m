function kl = KLdiv(q,p)
%KLdiv Compute the Kullback-Leibler divergence between distributions q and
%p expressed as potentials
%
%  kl = KLdiv(q,p)
entropic = table(sumpot(multpots([q logpot(q)]),[],0));
energic = table(sumpot(multpots([q logpot(p)]),[],0));
kl = entropic-energic;