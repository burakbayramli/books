function y = shrinkage(a, kappa)
    y = max(0, a-kappa) - max(0, -a-kappa);
end
