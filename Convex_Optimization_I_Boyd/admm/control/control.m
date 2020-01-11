function h = control

n = 50; % decision variables
m = 5;  % PWL components

tic

A = randn(m,4*n,8);
b = randn(m,1);

f = @(u,A) max(A*u(:) + b);

MAX_ITER = 100;
ABSTOL   = 1e-4;
RELTOL   = 1e-2;

lambda = 1;

x_aaa = zeros(n,4); x_aab = zeros(n,4); x_aba = zeros(n,4); x_abb = zeros(n,4);
x_baa = zeros(n,4); x_bab = zeros(n,4); x_bba = zeros(n,4); x_bbb = zeros(n,4);

z_0   = zeros(n,1);
z_a   = zeros(n,1); z_b   = zeros(n,1);
z_aa  = zeros(n,1); z_ab  = zeros(n,1); z_ba  = zeros(n,1); z_bb  = zeros(n,1);
z_aaa = zeros(n,4); z_aab = zeros(n,4); z_aba = zeros(n,4); z_abb = zeros(n,4);
z_baa = zeros(n,4); z_bab = zeros(n,4); z_bba = zeros(n,4); z_bbb = zeros(n,4);
z = [ z_aaa z_aab z_aba z_abb z_baa z_bab z_bba z_bbb ];

u_aaa = zeros(n,4); u_aab = zeros(n,4); u_aba = zeros(n,4); u_abb = zeros(n,4);
u_baa = zeros(n,4); u_bab = zeros(n,4); u_bba = zeros(n,4); u_bbb = zeros(n,4);

tstart = tic;
tx = 0;

for k = 1:MAX_ITER

    % x-update
    titer = tic;
    x_aaa = prox_f(z_aaa - u_aaa, lambda, A(:,:,1), b);
    x_aab = prox_f(z_aab - u_aab, lambda, A(:,:,2), b);
    x_aba = prox_f(z_aba - u_aba, lambda, A(:,:,3), b);
    x_abb = prox_f(z_abb - u_abb, lambda, A(:,:,4), b);
    x_baa = prox_f(z_baa - u_baa, lambda, A(:,:,5), b);
    x_bab = prox_f(z_bab - u_bab, lambda, A(:,:,6), b);
    x_bba = prox_f(z_bba - u_bba, lambda, A(:,:,7), b);
    x_bbb = prox_f(z_bbb - u_bbb, lambda, A(:,:,8), b);
    tx = tx + toc(titer);

    % z-update
    % Note: should strictly be x + u, but u's sum to zero, so are omitted.
    z_0 = avg(x_aaa(:,1), x_aab(:,1), x_aba(:,1), x_abb(:,1), ...
              x_baa(:,1), x_bab(:,1), x_bba(:,1), x_bbb(:,1));

    z_a = avg(x_aaa(:,2), x_aab(:,2), x_aba(:,2), x_abb(:,2));
    z_b = avg(x_baa(:,2), x_bab(:,2), x_bba(:,2), x_bbb(:,2));

    z_aa = avg(x_aaa(:,3), x_aab(:,3));
    z_ab = avg(x_aba(:,3), x_abb(:,3));
    z_ba = avg(x_baa(:,3), x_bab(:,3));
    z_bb = avg(x_bba(:,3), x_bbb(:,3));

    z_aaa = [z_0 z_a z_aa x_aaa(:,4)];
    z_aab = [z_0 z_a z_aa x_aab(:,4)];
    z_aba = [z_0 z_a z_ab x_aba(:,4)];
    z_abb = [z_0 z_a z_ab x_abb(:,4)];
    z_baa = [z_0 z_b z_ba x_baa(:,4)];
    z_bab = [z_0 z_b z_ba x_bab(:,4)];
    z_bba = [z_0 z_b z_bb x_bba(:,4)];
    z_bbb = [z_0 z_b z_bb x_bbb(:,4)];

    % u-update
    u_aaa = u_aaa + x_aaa - z_aaa;
    u_aab = u_aab + x_aab - z_aab;
    u_aba = u_aba + x_aba - z_aba;
    u_abb = u_abb + x_abb - z_abb;
    u_baa = u_baa + x_baa - z_baa;
    u_bab = u_bab + x_bab - z_bab;
    u_bba = u_bba + x_bba - z_bba;
    u_bbb = u_bbb + x_bbb - z_bbb;

    % diagnostics, reporting, termination checks
    x = [ x_aaa x_aab x_aba x_abb x_baa x_bab x_bba x_bbb ];
    zold = z;
    z = [ z_aaa z_aab z_aba z_abb z_baa z_bab z_bba z_bbb ];
    u = [ u_aaa u_aab u_aba u_abb u_baa u_bab u_bba u_bbb ];

    h.r_norm(k)   = norm(x - z,'fro');
    h.s_norm(k)   = norm(-(z - zold)/lambda,'fro');
    h.eps_pri(k)  = sqrt(numel(x))*ABSTOL + RELTOL*max(norm(x,'fro'), norm(-z,'fro'));
    h.eps_dual(k) = sqrt(numel(x))*ABSTOL + RELTOL*norm(u/lambda,'fro');

    h.objval(k)   = (1/8)*(f(z_aaa,A(:,:,1)) + f(z_aab,A(:,:,2)) + f(z_aba,A(:,:,3)) + ...
                           f(z_abb,A(:,:,4)) + f(z_baa,A(:,:,5)) + f(z_bab,A(:,:,6)) + ...
                           f(z_bba,A(:,:,7)) + f(z_bbb,A(:,:,8)));

    if mod(k,10) == 0
        fprintf('%3d\t%10.4f\t%10.4f\t%10.4f\t%10.4f\n', k, ...
            h.r_norm(k), h.eps_pri(k), h.s_norm(k), h.eps_dual(k));
    end

    if h.r_norm(k) < h.eps_pri(k) && h.s_norm(k) < h.eps_dual(k)
         break;
    end

end

tx/k
toc(tstart)


end

