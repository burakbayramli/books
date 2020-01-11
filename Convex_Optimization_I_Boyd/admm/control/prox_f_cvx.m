function x = prox_f_cvx(v, lambda, f, A)
% For testing purposes.
    [n T] = size(v);
    cvx_begin quiet
        variable x(n,T)
        minimize(f(x,A) + (1/(2*lambda))*square_pos(norm(x - v,'fro')))
        subject to
            x <= 1;
            x >= -1;
    cvx_end
end
