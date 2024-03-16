function q = vortex_semiinf (r, v, l)
  q = vortex_segment (r, v, v + 1e3 * l);
