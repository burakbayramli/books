function m = viscosity (T)
  m = 1.495e-6 * sqrt (T) ./ (1 + 120 ./ T);
