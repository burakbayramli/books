function [F] = CutOffFilter2D(Nc,frac)

% function [F] = CutOffFilter2D(Nc,frac)
% Purpose : Initialize 2D cut off filter matrix of order Norderin

Globals2D;

filterdiag = ones(Np,1);

% build exponential filter
sk = 1;
for i=0:N
  for j=0:N-i
    if (i+j>=Nc)
      filterdiag(sk) = frac;
    end
    sk = sk+1;
  end
end

F = V*diag(filterdiag)*invV;
return;
