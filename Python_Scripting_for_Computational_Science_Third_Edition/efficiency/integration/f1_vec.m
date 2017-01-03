function y = f1_vec(x)
y = exp(-x .* x) .* log(1 + x .* sin(x));

