N = 100*(2.^(0:4));
plot_res = 100;
T = 1;
l = -1;
r = 3;

f = @(x) x.^2 / 2;
f_prime = @(x) x;
f_prime_inv = @(x) x;
f_type = 'convex';
f_extrema = 0;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

u0 = shock();
s = 1/2;
u_e = @(x) x < s*T;
ic_str = 'shock';
axis_limits = [1e2 2e3 1e-3 1e-1];
plotCase(ic_str, N, plot_res, axis_limits, T, l, r, f, f_prime, f_type, f_extrema, u0, u_e)
print -djpg '/tmp/plot_shock.jpg'

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

u0 = rarefaction();
u_l = -1;
u_r = 1;
u_e = @(x) getRarefaction(x, u_l, u_r, f_prime, f_prime_inv,T);
ic_str = 'rarefaction';
axis_limits = [1e2 2e3 1e-2 2e-0];
plotCase(ic_str, N, plot_res, axis_limits, T, l, r, f,f_prime, f_type, f_extrema, u0, u_e)
print -djpg '/tmp/plot_rarefaction.jpg'

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

u0 = rare_shock();
s = 1/2;
u_l = 0;
u_r = 1;
u_e = @(x) (x>=1).*((x-1) < s*T) + (x<1).*getRarefaction(x, u_l, u_r, f_prime, f_prime_inv, T);

ic_str = 'rarefaction and shock';
axis_limits = [1e2 2e3 0.2e-2 1e-0];
plotCase(ic_str, N, plot_res, axis_limits, T, l, r, f,f_prime, f_type, f_extrema, u0, u_e)
print -djpg '/tmp/plot_rarefaction-shock.jpg'

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



