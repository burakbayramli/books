
function maxleb = Lebesgue2D(N, r, s, Nsamples, Nlevels)

  V = Vandermonde2D(N, r, s);

  lcoeffs = inv(V);

  M = Nsamples;

  % randomly sample lebesgue function
  L1 = rand(M,1);
  L2 = rand(M,1);
  L3 = 1-L1-L2;
  ids = find(L3>0);
  L1 = L1(ids);
  L2 = L2(ids);
  L3 = L3(ids);
  
  h = 1;

  for cnt=1:Nlevels
    f = lebfn2d(N, lcoeffs, L1, L2, L3);
    
    [fsort, ids] = sort(-f);
    ids = ids(1:(M/5));

    L1 = L1(ids);
    L2 = L2(ids);
    L3 = L3(ids);
    
    % now perturb again
    h = h/2;
    L1new = L1*ones(1, 20) + h*(2*rand(length(L1),20)-1);
    L2new = L2*ones(1, 20) + h*(2*rand(length(L2),20)-1);
    L3new = 1-L1new-L2new;
    
    L1 = [L1(1:100);L1new(:)];
    L2 = [L2(1:100);L2new(:)];
    L3 = [L3(1:100);L3new(:)];
    
    ids = find( (L1+L2+L3)<=1  & L1>=0 & L2>=0 & L3>=0);
    L1 = L1(ids);
    L2 = L2(ids);
    L3 = L3(ids);
    
  end
  
  
  f = lebfn2d(N, lcoeffs, L1, L2, L3);

  [fsort, ids] = sort(-f);
  ids = ids(1);
  maxleb = f(ids(1))
  
