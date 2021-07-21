function c = capital(c0, r, n)
% capital(c0, r, n)
%
% Computes the resulting capital c of the initial investment c0, the annual
% interest rate r and the number of years n
  c = c0 * (1 + r)^n;
