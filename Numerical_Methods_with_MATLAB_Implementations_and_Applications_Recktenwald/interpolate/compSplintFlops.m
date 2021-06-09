function compSplintFlops
% compSplintFlops  Show flops savings due to sparse storage in spline interp
%
% Synopsis:  compSplintFlops
%
% Input:  None
%
% Output:  Table of flops to compute spline interpolant

fprintf('            sparse      full    relative\n');
fprintf(' knots      flops       flops    effort\n');

nk = [4 8 16 32 64 128 256 512];
for k = 1:length(nk)
  x = linspace(0,5,nk(k))';  %  Generate discrete data set
  y = x.*exp(-x);
  xi = mean(x);              %  Evaluate spline at this xi
  flops(0);                  %  Reset flops counter
  yi = splint(x,y,xi);       %  Spline with not-a-knot end conditions
  fs(k) = flops;
  
  flops(0);                  %  Reset flops counter
  yi = splintFull(x,y,xi);   %  Repeat with full storage
  ff(k) = flops;
  fprintf('%5d  %10d  %10d  %8.4f\n',nk(k),fs(k),ff(k),fs(k)/ff(k));
end

loglog(nk,fs,'ro',nk,ff,'bs');
xlabel('knots');   ylabel('flops');
legend('sparse','full',2);

