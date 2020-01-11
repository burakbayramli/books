function x = prox_f(v, lambda, A, b)
% CVXGEN-based solver.
    [n T] = size(v);
    params.A_0 = A(:,1:n);
    params.A_1 = A(:,n+1:2*n);
    params.A_2 = A(:,2*n+1:3*n);
    params.A_3 = A(:,3*n+1:4*n);
    params.lambda = lambda;
    params.b   = b;
    params.v_0 = v(:,1);
    params.v_1 = v(:,2);
    params.v_2 = v(:,3);
    params.v_3 = v(:,4);
    settings.verbose = 0;
    vars = csolve(params, settings);
    x = [vars.x_0 vars.x_1 vars.x_2 vars.x_3];
end

