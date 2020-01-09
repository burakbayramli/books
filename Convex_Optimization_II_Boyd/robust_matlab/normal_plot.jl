# normal_plot
#
# Plots inverse normal CDF versus sqrt(2 log (1 / epsilon)) as epsilon
# goes to zero.

module normal_plot
using PyPlot;

# normal_cdf(x)
#
# For Z distributed as standard normal, computes and returns
# P(Z <= x).
function normal_cdf(x)
  return .5 + .5 * erf(x / sqrt(2));
end

# normal_cdf_inv(t)
#
# For Z distributed as standard normal, computes and returns the x such that
# P(Z <= x) = t.
function normal_cdf_inv(t)
  return -sqrt(2) * erfinv(1 - 2 * t);
end

# plot_error_approximations(min_epsilon = 1e-6)
#
# Plots the approximations for error used in the
# moment-generating-function bound for normals versus the true error
# bound.
function plot_error_approximations(min_epsilon = 1e-6)
  epsilons = logspace(log10(.5), log10(min_epsilon), 25);
  true_inverse_probs = normal_cdf_inv(1 - epsilons);
  fake_inverse_probs = sqrt(2 * log(1 ./ epsilons));
  figure();
  semilogx(epsilons, true_inverse_probs, "ko-", linewidth=2,
           label = "            ");
  semilogx(epsilons, fake_inverse_probs, "b+-", linewidth=2,
           markersize=8, markeredgewidth=1.5,
           label = "            ");
  legend();
  savefig("bad_normal_probs.eps", bbox_inches="tight");
  savefig("bad_normal_probs.pdf", bbox_inches="tight");
end

end  # module normal_plot