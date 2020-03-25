function x=nbinrnd(r,p)
% uniformly fast NegBin(r,p) generator (Algorithm 4.11)
% vectors r and p have to be of the same size

x=nan(size(r));
for i=1:length(r)
    Lam=gamrand(r(i),p(i)/(1-p(i)));
    x(i)=poisrnd_atk(Lam);
end