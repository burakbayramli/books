function p = objective(A, b, lambda, N, x, z)
    p = ( 1/2*sum_square(N*z - b) + lambda*sum(norms(x)) );
end

