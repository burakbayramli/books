# bounded_rvs_mgf
#
# Compares solution quality for a Monte-Carlo simulated version of the
# conditional value at risk constraint to that given by direct moment
# generating function calculations. Generates a histogram of solutions.

using PyPlot;
using Convex;
using ECOS;

srand(1);

n = 15;  # Dimensions
m = 2 * 10^3;  # Monte carlo simulations
mu = 1.05 + .3 ./ (1:n);  # Means for random variables
epsilon = .1;  # Probability of failing to satisfy constraint
ranges = .6 ./ (1:n);

# Construct objective for analytic and MC problem
t_analytic = Variable(1);
x_analytic = Variable(n);
obj_analytic = maximize(t_analytic);
obj_analytic.constraints += (t_analytic - mu' * x_analytic +
                             sqrt(log(1 / epsilon) / 2)
                             * norm(2 * ranges .* x_analytic, 2) <= 0);
obj_analytic.constraints += (x_analytic >= 0);
obj_analytic.constraints += (sum(x_analytic) == 1);

# Random problem
t_mc = Variable(1);
x_mc = Variable(n);
alpha_mc = Variable(1);
obj_mc = maximize(t_mc);
Uvals = repmat(mu, 1, m) + repmat(ranges, 1, m) .* sign(1 - 2 * rand(n, m));
obj_mc.constraints += ((1 / m) * sum(max(t_mc - x_mc' * Uvals + alpha_mc, 0))
                       - alpha_mc * epsilon <= 0);
obj_mc.constraints += (x_mc >= 0);
obj_mc.constraints += (sum(x_mc) == 1);

solve!(obj_analytic, ECOSSolver(maxit=1000));
solve!(obj_mc, ECOSSolver(maxit=1000));

# Now do a new monte carlo, see how many times we violate the constraint
srand(5);

m = 5 * 10^4;
Rs = repmat(mu, 1, m) + repmat(ranges, 1, m) .* sign(1 - 2 * rand(n,m));

ta = t_analytic.value;
values_analytic = Rs' * x_analytic.value;
num_analytic_violations = sum(ta .> values_analytic);

tmc = t_mc.value;
values_mc = Rs' * x_mc.value;
num_mc_violations = sum(tmc .> values_mc);

figure();

PyPlot.plt.hist([values_analytic'; values_mc']', bins = 15,
                color = ([0, .5, 1], [1, .5, 0]),
                label = ("        ", "        "));

# Relabel y axis
ytick_vals = yticks()[1];
proportions = ytick_vals / m;
yticks(ytick_vals, proportions, fontsize=14);

(miny, maxy) = axis()[3:4];
plot([ta, ta], [miny, maxy], linewidth=2, color=[0, .25, .5]);
plot([tmc, tmc], [miny, maxy], linewidth=2, color = [.5, .25, 0]);
legend();

savefig("mgf_vs_mc_portfolio.eps", bbox_inches="tight");
savefig("mgf_vs_mc_portfolio.pdf", bbox_inches="tight");

println("Fraction of violations under analytic: ",
        num_analytic_violations / m,
        "\nFraction of violations under Monte Carlo: ",
        num_mc_violations / m);

println("Mean return under analytic: ", sum(mu .* x_analytic.value),
        "\nMean return under Monte Carlo: ",
        sum(mu .* x_mc.value));
