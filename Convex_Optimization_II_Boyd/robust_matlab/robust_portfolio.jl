# robust_portfolio.jl
#
# Solves three portfolio problems described in the robust_note.tex.
#
# Author: jduchi@stanford.edu (John Duchi)

using Convex;
using ECOS;
using PyPlot;
srand(1);

n = 200;
mu = 1.05 + .3 * linspace(1, 1/n, n);
u_max = .05 + .6 * linspace(1, 1/n, n);  # Uncertainty sets
u_max[n] = 0;  # No variability on last coordinate
epsilon = 2e-4;  # Risk parameter (probability of failure)

x_nom = [1, zeros(n-1)];  # Nominal solution (best return)
x_con = [zeros(n-1), 1];  # Conservative solution

x_rob_cvx = Variable(n);
z_cvx = Variable(n);
t_cvx = Variable(1);
port_prob = maximize(mu' * x_rob_cvx - t_cvx);
port_prob.constraints += (sqrt(2 * log(1/epsilon)) * norm(z_cvx, 2) <= t_cvx);
port_prob.constraints += (z_cvx == u_max .* x_rob_cvx);
port_prob.constraints += (sum(x_rob_cvx) == 1);
port_prob.constraints += (x_rob_cvx >= 0);

solve!(port_prob, ECOSSolver(maxit=100, verbose=false));

x_rob = x_rob_cvx.value;

num_samples = 2 * 10^4;
returns_nom = zeros(num_samples);
returns_con = zeros(num_samples);
returns_rob = zeros(num_samples);
for sample = 1:num_samples
  R = mu + u_max .* (1 - 2 * rand(n));
  returns_nom[sample] = sum(R .* x_nom);
  returns_con[sample] = sum(R .* x_con);
  returns_rob[sample] = sum(R .* x_rob);
end

println("Value at risk ", epsilon, " is at least ",
        sum(mu' * x_rob) - sqrt(2 * log(1/epsilon)) * norm(u_max .* x_rob));

figure();
PyPlot.plt.hist(returns_nom, bins = 16, color = [.5, .4, 1],
                label = " ");
PyPlot.plt.hist(returns_con, bins = [mu[n]-1e-2, mu[n] + 1e-2],
                color = [.9, .6, .4], label = "        ");
PyPlot.plt.hist(returns_rob, bins = 14,
                color = [.1, .1, .8], label = "        ");
axis([.6, 2, 0, num_samples/2]);
legend();
savefig("portfolio_returns.eps", bbox_inches="tight");
savefig("portfolio_returns.pdf", bbox_inches="tight");
