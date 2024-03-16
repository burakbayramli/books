function q = vortex_horseshoe (r, r1, r2, lhat)
  vsize = [1, 1, size(r, 2)];
  R = repmat (r, [1, 1, size(r1, 2)]);
  R1 = permute (repmat (r1, vsize), [1, 3, 2]);
  R2 = permute (repmat (r2, vsize), [1, 3, 2]);
  LHAT = permute (repmat (lhat, vsize), [1, 3, 2]);
  q = vortex_segment (R, R1, R2) ...
      - vortex_semiinf (R, R1, LHAT) ...
      + vortex_semiinf (R, R2, LHAT)
