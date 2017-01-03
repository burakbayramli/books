function xs = inv_chi2_rnd(v, s2, m, n)
% Draw an mxn matrix of inverse chi squared rvs, v = dof, s2=scale
% Gelman p580
xs = v*s2./chi2rnd(v, m, n);


