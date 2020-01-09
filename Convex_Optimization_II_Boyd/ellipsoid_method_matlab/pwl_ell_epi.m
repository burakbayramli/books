DEEPCUT = 1;
pwl_ellip;
fs = f_best;
pwl_epiellip;

figure(1);
cla;
set(gca, 'FontSize',20);
M = 2000;
x = 1:M;
semilogy(x, f_best(1:M) - cvx_optval)
hold on
semilogy(x, fs(1:M) - cvx_optval, 'k--')

axis([0 M 1e-4 1e0]);
xlabel('k');
ylabel('fbest - fmin');
legend('epigraph method', 'non-epigraph deep cuts');

print -deps pwl_ell_epi.eps
