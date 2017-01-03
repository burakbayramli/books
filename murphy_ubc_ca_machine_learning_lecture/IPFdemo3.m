% IPFdemo3
% Fit loopy MRF 1-2-3-1 using iterative proportional fitting

clqs = {[1 2], [2 3], [1 3]};
NC = length(clqs);
N = 3;

% Some count data
C = reshape([53 414 11 37 0 16 4 139], [2 2 2]);
C = normalise(C);
Cpot = tabularPot(1:N, 2*ones(1,N), C);
for c=1:NC
  counts{c} = marginalizePot(Cpot, clqs{c});
end

% Initial guess is all 1's
for c=1:NC
  pots{c} = tabularPot(clqs{c}, 2*ones(1,length(clqs{c})));
end
converged = 0;
iter = 0;
thresh = 1e-3; % convergence threshold
while ~converged
  converged = 1;
  for c=1:NC
    potsOld{c} = pots{c};
  end
  iter = iter + 1;
  fprintf('iter %d\n', iter);
  for c=1:NC
    J = multiplyPots(pots{:});
    Mc = marginalizePot(J, clqs{c});
    pots{c}.T = pots{c}.T .* (counts{c}.T ./ Mc.T);
    if ~approxeq(pots{c}.T, potsOld{c}.T, thresh)
      converged = 0;
    end
    fprintf('c=%d\n', c)
    printTable(pots{c})
  end
end

J = multiplyPots(pots{:});
for c=1:NC
  Mc = marginalizePot(J, clqs{c});
  assert(approxeq(counts{c}.T, Mc.T))
end

