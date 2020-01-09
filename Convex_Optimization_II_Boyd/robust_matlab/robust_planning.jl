# robust_planning.jl
#
# Shows example due to Ben-Tal et al. on a very non-robust linear program.

using Convex;
using ECOS;
using PyPlot;

srand(1);

c = [100, 199.9, -5500, -6100];
A = [-.01 -.02 .5 .6;
     1 1 0 0;
     0 0 90 100;
     0 0 40 50;
     100 199.9 700 800;
     -eye(4)];
b = [0, 1000.0, 2000, 800, 100000, 0, 0, 0, 0];

(m, n) = size(A);

x_non_robust = Variable(4);
prob = minimize(c' * x_non_robust);
prob.constraints += (A * x_non_robust <= b);
solve!(prob, ECOSSolver(maxit=1000, verbose=false));

nominal_value = prob.optval;

# Robust version

x_robust = Variable(4);
prob = minimize(c' * x_robust);
prob.constraints += (A[1, :] * x_robust +
                     .005 * abs(A[1,1]) * abs(x_robust[1]) +
                     .02 * abs(A[1, 2]) * abs(x_robust[2]) <= b[1]);
for i = 2:m
  prob.constraints += (A[i, :] * x_robust <= b[i]);
end
solve!(prob, ECOSSolver(maxit=1000, verbose=false));

abs_loss = abs(sum(c .* x_robust.value) - sum(c .* x_non_robust.value));
println("Absolute loss from robust: ", abs_loss,
        ", relative loss: ", abs_loss / abs(sum(c .* x_non_robust.value)));

# Now, do some random perturbations of the data by 1% and see how much
# we get out of whack.
num_tests = 10^4;
absolute_changes = zeros(num_tests);
relative_changes = zeros(num_tests);

for i = 1:num_tests
  x_val = x_non_robust.value;
  delta_1 = (5e-5) * (1 - 2 * rand());
  delta_2 = (4e-4) * (1 - 2 * rand());
  if (delta_2 > 0)
    # Must reduce output of x[3] to handle.
    x_val[3] = -(A[1, 2] + delta_2) * x_val[2] / A[1, 3];
  end
  absolute_changes[i] = sum(c .* x_val) - nominal_value;
  relative_changes[i] = abs(absolute_changes[i]) / abs(nominal_value);
end

figure();
PyPlot.plt.hist(relative_changes, bins=16);
savefig("pricing_changes.eps", bbox_inches="tight");
savefig("pricing_changes.pdf", bbox_inches="tight");
