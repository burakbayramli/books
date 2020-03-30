function DM = ProductKernel(dsites,ctrs,K)
  
  [M,d] = size(dsites); [N,d] = size(ctrs);
  DM = ones(M,N);
  for ell=1:d
     DM = DM .* K(repmat(dsites(:,ell),1,N),repmat(ctrs(:,ell)',M,1));
  end
end
