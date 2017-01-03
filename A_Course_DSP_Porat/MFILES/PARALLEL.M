function y = parallel(c,nsec,dsec,x);
% Synopsis: y = parallel(c,nsec,dsec,x).
% Parallel realization of an IIR digital filter.
% Input parameters:
% c: the free term of the filter.
% nsec, dsec: numerators and denominators of second-order sections
% x: the input sequence.
% Output:
% y: the output sequence.

% Part of software package for the book:
% A Course in Digital Signal Processing
% by Boaz Porat, John Wiley & Sons, 1997

[n,m] = size(dsec); dsec = dsec(:,2:3);
u = zeros(n,2); % u: the internal state
for i = 1:length(x),
   y(i) = c*x(i);
   for k = 1:n,
      unew = x(i)-sum(u(k,:).*dsec(k,:)); u(k,:) = [unew,u(k,1)];
      y(i) = y(i) + sum(u(k,:).*nsec(k,:));
   end
end

