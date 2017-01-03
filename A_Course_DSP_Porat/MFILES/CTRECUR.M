function X = ctrecur(x,W);
% Synopsis: X = ctrecur(x,W).
% Cooley-Tukey frequency-decimation recursion.
% Input parameters:
% x: the input vector, assumed to be a row
% W: vector of powers of W, assumed to be a row.
% Output:
% y: the output row vector.

% Part of software package for the book:
% A Course in Digital Signal Processing
% by Boaz Porat, John Wiley & Sons, 1997

N = length(x); Q = N;
for i = 2:floor(sqrt(N)),
   if (rem(N,i) == 0), Q = i; break, end, end

if (Q == N), X = primedft(x,W);
else,
   P = N/Q; tmp = reshape(x,P,Q);
   for p = 0:P-1,
   tmp(p+1,:) = primedft(tmp(p+1,:),W(1:P:N));
   if (p > 0), tmp(p+1,2:Q) = tmp(p+1,2:Q).*W(rem(p*(1:Q-1),N)+1); end
   end
   for q = 1:Q, tmp(:,q) = (ctrecur(tmp(:,q).',W(1:Q:N))).'; end
   X = reshape(tmp.',1,N);
end

