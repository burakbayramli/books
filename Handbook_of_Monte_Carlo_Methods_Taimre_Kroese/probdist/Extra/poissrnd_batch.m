function x=poissrnd_batch(lam,p)
% generates x_i~Poi(lam*p_i) for i=1,...,m,
% independently using a batch generation method (algorithm 4.16);

N=poissrnd_atk(lam);
x=mnrand(N,p);


  
  