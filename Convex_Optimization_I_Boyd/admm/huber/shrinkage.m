function z = shrinkage(x, kappa)
    z = huber_pos(1 - kappa./abs(x)).*x;
end

