function u = normalize (v)
  u = bsxfun (@rdivide, v, sqrt (dot (v, v)));
