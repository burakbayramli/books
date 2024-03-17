function [p, T, rho, a, mu] = atmosphere (y)
  g = 9.80665; R = 287.0; cpcv = 7/5;
  T0 = 15 + 273.15; p0 = 101325e0;
  L = 6.5e-3; yt = 11e3; top = 20e3;
  troposphere = y <= yt;
  strat =  ̃troposphere & (y <= top);
  T = NaN (size (y)); p = T;
  T(troposphere) = T0 - L * y(troposphere);
  Ts = T0 - L * yt;
  T(strat) = Ts;
  p(troposphere) = p0 * (T(troposphere) / T0) .ˆ (g/L/R);
  pt = p0 * (Ts / T0) ˆ (g/L/R);
  p(strat) = pt * exp (g/R/Ts * (yt - y(strat)));
  rho = p ./ (R * T);
  a = speed of sound (T);
  mu = viscosity (T);
