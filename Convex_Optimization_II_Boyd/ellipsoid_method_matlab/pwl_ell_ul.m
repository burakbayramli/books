DEEPCUT = 0;
pwl_ellip;

figure(1);
cla;
set(gca, 'FontSize',18);
M = 200;
x = 1:M;
plot(x, f_save(1:M))
hold on;
plot(x, Lm(1:M))
plot([x(1) x(end)], [cvx_optval cvx_optval], 'k--');

axis([0 M -8 4]);
text(x(27), f_save(27), 'u');
text(x(27), Lm(27), 'l');
text(-2, cvx_optval, 'fs');
set(gca, 'YTickLabel', []);
set(gca,'XTickLabel', ['a'; 'b'; 'c'; 'd'; 'e']);
% 0 500 1000 1500 2000
xlabel('k');

print -deps pwl_ell_ul1.eps

figure(2);
cla;
set(gca, 'FontSize',18);
M = 2000;
x = 1:M;
semilogy(x, f_best(1:M) - cvx_optval)

axis([0 M 1e-4 1e0]);
xlabel('k');
ylabel('fbest - fmin');

print -deps pwl_ell_ul2.eps

figure(3);
cla;

set(gca, 'FontSize',18);
M = 2000;
x = 1:M;
plot(x, U(1:M))
hold on;
plot(x, L(1:M))
plot([x(1) x(end)], [cvx_optval cvx_optval], 'k--');

axis([0 M -3 3]);
text(x(300), U(300), 'u');
text(x(300), L(300), 'l');
text(-20, cvx_optval, 'fs');
set(gca, 'YTickLabel', []);
set(gca,'XTickLabel', ['a'; 'b'; 'c'; 'd'; 'e']);
xlabel('k');

print -deps pwl_ell_ul3.eps
