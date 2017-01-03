function s = sctest(a);
% Synopsis: s = sctest(a).
% Schur-Cohn stability test.
% Input:
% a: coefficients of polynomial to be tested.
% Output:
% s: 1 if stable, 0 if unstable.

% Part of software package for the book:
% A Course in Digital Signal Processing
% by Boaz Porat, John Wiley & Sons, 1997

n = length(a);
if (n == 1), s = 1; % a zero-order polynomial is stable
else,
   a = reshape((1/a(1))*a,1,n); % make the polynomial monic
   if (abs(a(n)) >= 1), s = 0; % unstable
   else,
      s = sctest(a(1:n-1)-a(n)*fliplr(a(2:n))); % recursion
   end
end

