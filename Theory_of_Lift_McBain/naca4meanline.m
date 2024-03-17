function y = naca4meanline (x, m, p)
  aft = x(:) >= p;
  y( ̃aft,1) = m / pˆ2 * (2*p*x( ̃aft) - x( ̃aft) .ˆ2);
  y(aft,1) = m / (1-p)ˆ2 * (1-2*p + 2*p*x(aft) - x(aft).ˆ2);
