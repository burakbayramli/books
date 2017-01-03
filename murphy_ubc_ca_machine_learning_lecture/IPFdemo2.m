% IPFdemo2
% Fit an MRF 1-2-3 using iterative proportional fitting

C = reshape([53 414 11 37 0 16 4 139], [2 2 2]);
C = normalise(C)
C12 = sumv(C,3);
C23 = sumv(C,1);
C13 = sumv(C,2);

psi12 = tabularPot([1 2], [2 2]);
psi23 = tabularPot([2 3], [2 2]);
 
for iter=1:2
  fprintf('iter %d\n', iter);
  J = multiplyPots(psi12, psi23);
  M12 = marginalizePot(J, [1 2]);
  psi12.T = psi12.T .* (C12 ./ M12.T);
  fprintf('psi12\n');
  printTable(psi12)
  
  J = multiplyPots(psi12, psi23);
  M23 = marginalizePot(J, [2 3]);
  psi23.T = psi23.T .* (C23 ./ M23.T);
  fprintf('psi23\n');
  printTable(psi23)
end

J = multiplyPots(psi12, psi23);
M12 = marginalizePot(J, [1 2]);
M23 = marginalizePot(J, [2 3]);
assert(approxeq(C12, M12.T))
assert(approxeq(C23, M23.T))


