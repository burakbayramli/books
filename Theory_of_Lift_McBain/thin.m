function [alpha0, Cmac] = thin (x, y)
  chi = acos (2 * x - 1);
  slope = diff (y) ./ diff (x);
  alpha0 = -slope’ * diff (chi + sin (chi)) / pi;
  Cmac = -slope’ * diff (sin (chi) + sin (2 * chi) / 2) / 2;
