# robust_regression.jl
#
# Solves a couple of robust optimization problems with regression as
# example. In particular, looks at solving
#
# min. sup_{Delta in U} norm((A + Delta) * x - b),
#
# where U is one of the uncertainty sets given by
#
# Uinf = { norm(Delta[:], Inf) <= t_inf(delta) }
# U2   = { norm(Delta[i, :], 2) <= sqrt(n) + t_2(delta) }
# Uop  = { norm(Delta) <= sqrt(n) + sqrt(m) + t_op(delta) }
#
# t_inf(delta) = sqrt(2 * log(2 * m * n / delta)),
# t_2(delta)   = sqrt(2 * log(m / delta))
# t_op(delta)  = sqrt(2 * log(1 / delta)).
#
# The data matrix A is generated as an m-by-n matrix of i.i.d. N(0, 100)
# random variables, and the vector b is A * u + noise for a vector u
# uniform on the unit sphere and noise i.i.d. N(0, 1).
#
# Author: jduchi@stanford.edu (John Duchi)

using Convex;
using ECOS;
using PyPlot;
srand(1);

# Generate data
m = 50;
n = 20;
A = 10 * randn(m, n);
u = randn(n); u = u / norm(u);
b = A * u + randn(m);

# These deltas are probabilities of exceeding the bounds on robustness.
num_deltas = 23;
deltas = logspace(-11, -1, num_deltas);

x_inf_sols = zeros(n, num_deltas);
x_2_sols = zeros(n, num_deltas);
x_op_sols = zeros(n, num_deltas);

# Nominal values
nom_vals_inf = zeros(num_deltas);
nom_vals_2 = zeros(num_deltas);
nom_vals_op = zeros(num_deltas);

for ind = 1:num_deltas
  delta = deltas[ind];
  t_op = sqrt(n) + sqrt(m) + sqrt(2 * log(1 / delta));
  t_2 = sqrt(n) + sqrt(2 * log(m / delta));
  t_inf = sqrt(2 * log(2 * m * n / delta));
  # Solve each of the robust formulations
  println("Solving iteration ", ind, " of ", num_deltas, " with delta = ",
          delta);
  # First with infinity norm-bounded perturbations
  x_inf = Variable(n);
  z_inf = Variable(m);
  problem_inf = minimize(norm(z_inf, 2));
  for i = 1:m
    problem_inf.constraints += (z_inf[i] >= abs(A[i, :] * x_inf - b[i])
                                + t_inf * norm(x_inf, 1));
  end
  solve!(problem_inf, ECOSSolver(maxit = 1000, verbose=false));
  x_inf_sols[:, ind] = x_inf.value;
  # Now with ellipsoid (l2) perturbations
  x_2 = Variable(n);
  z_2 = Variable(m);
  problem_2 = minimize(norm(z_2, 2));
  for i = 1:m
    problem_2.constraints += (z_2[i] >= abs(A[i, :] * x_2 - b[i])
                              + t_2 * norm(x_2, 2));
  end
  solve!(problem_2, ECOSSolver(maxit = 1000, verbose=false));
  x_2_sols[:, ind] = x_2.value;
  # Now with the operator norm perturbation, which is equivalent to solving
  # min. ||A x - b||_2 + t_op * ||x||_2.
  x_op = Variable(n);
  problem_op = minimize(norm(A * x_op - b, 2) + t_op * norm(x_op, 2));
  solve!(problem_op, ECOSSolver(maxit = 1000, verbose=false));
  x_op_sols[:, ind] = x_op.value;
end

for ind = 1:num_deltas
  nom_vals_inf[ind] = norm(A * x_inf_sols[:, ind] - b, 2);
  nom_vals_2[ind] = norm(A * x_2_sols[:, ind] - b, 2);
  nom_vals_op[ind] = norm(A * x_op_sols[:, ind] - b, 2);
end

x_nominal = A\b;
nominal_value = norm(A * x_nominal - b);
figure();
loglog(deltas, nom_vals_inf - nominal_value, "b-", linewidth=2,
       label = "        ");
loglog(deltas, nom_vals_2 - nominal_value, "k+-", linewidth=2,
       label = "        ", markersize=8, markeredgewidth=2);
loglog(deltas, nom_vals_op - nominal_value, "go-", linewidth=2,
       label = "        ");
legend();
savefig("nom_vs_delta_reg.eps", bbox_inches="tight");
savefig("nom_vs_delta_reg.pdf", bbox_inches="tight");

# Now, draw random samples and see what the residual values are

total_monte_carlos = 2 * 10^4;

println("Drawing ", total_monte_carlos, " samples to see residuals");

delta_ind = int(floor(2 * num_deltas / 3));
delta = deltas[delta_ind];
x_op = x_op_sols[:, delta_ind];
x_2 = x_2_sols[:, delta_ind];
x_inf = x_inf_sols[:, delta_ind];

obj_op = zeros(total_monte_carlos);
obj_2 = zeros(total_monte_carlos);
obj_inf = zeros(total_monte_carlos);
obj_nom = zeros(total_monte_carlos);
for i = 1:total_monte_carlos
  G = 1.5 * randn(m, n);
  obj_op[i] = norm((A + G) * x_op - b);
  obj_2[i] = norm((A + G) * x_2 - b);
  obj_inf[i] = norm((A + G) * x_inf - b);
  obj_nom[i] = norm((A + G) * x_nominal - b);
end

figure();

min_bin = min(minimum(obj_op), minimum(obj_nom));
max_bin = max(maximum(obj_op), maximum(obj_nom));

PyPlot.plt.hist(obj_op, bins=linspace(min_bin, max_bin, 20),
                color=[0,1,0], alpha=.8, label ="         ");
PyPlot.plt.hist(obj_nom, bins=linspace(min_bin, max_bin, 20),
                color=[0,0,1], alpha = .4, label="         ");
legend();
savefig("robust_hist_good.eps", bbox_inches="tight");
savefig("robust_hist_good.pdf", bbox_inches="tight");

# Now plot the really terrible versions
figure();
PyPlot.plt.hist(obj_op, bins=linspace(min_bin, max_bin, 20),
                color=[0,1,0], alpha=.8, label ="         ");
PyPlot.plt.hist(obj_nom, bins=linspace(min_bin, max_bin, 20),
                color=[0,0,1], alpha = .4, label="         ");
PyPlot.plt.hist(obj_2, bins=10,
                color=[1,0,0], alpha=1, label ="         ");
PyPlot.plt.hist(obj_inf, bins=3,
                color=[.5,.5,.5], alpha = 1, label="         ");
legend();
savefig("robust_hist_all.eps", bbox_inches="tight");
savefig("robust_hist_all.pdf", bbox_inches="tight");
