# phase-retrieval.jl
#
# Methods for the phase retrieval problem with absolute value loss and
# with l2 loss.

module PR

using PyPlot;
using Convex;
using ECOS;

# PlotExamples

function PlotExamples()
  srand(0);
  n = 50;
  m = 200;
  num_tests = 10;
  maxiter_lone = floor(Int64, m / 2);
  maxiter_quad = 2*m;
  objs_quadratic = zeros(maxiter_quad + 1, num_tests);
  objs_lone = zeros(maxiter_lone + 1, num_tests);
  ## First, random initializations
  srand(0);
  for test_ind = 1:num_tests
    (A, b, xopt) = GeneratePhaseRetrievalData(m, n);
    x_init = randn(n);
    println("\t*** Lone Objective (", test_ind, " of ", num_tests, ") ***\n");
    (x_lone, objs) =
      LoneProxLinear(A, b, x_init = x_init, maxiter = maxiter_lone);
    objs_lone[1:length(objs), test_ind] = objs;
    println("\t*** Quadratic Objective (", test_ind, " of ",
            num_tests, ") ***\n");
    (x_quad, objs) =
      QuadraticProxLinear(A, b, x_init = x_init, maxiter = maxiter_quad);
    objs_quadratic[1:length(objs), test_ind] = objs;
  end
  # Now plot L1 results
  figure();
  longest_last_ind = 0;
  for test_ind = 1:num_tests
    last_ind = findlast(objs_lone[:, test_ind] .> 0);
    longest_last_ind = max(longest_last_ind, last_ind);
    semilogy(1:last_ind, objs_lone[1:last_ind, test_ind], "k-",
             linewidth=1);
  end
  axis([1, longest_last_ind, 1e-5, 1e3]);
  savefig("l1-pr-random-init.eps", bbox_inches="tight");
  # Plot L2 results
  figure();
  longest_last_ind = 0;
  for test_ind = 1:num_tests
    last_ind = findlast(objs_quadratic[:, test_ind] .> 0);
    longest_last_ind = max(last_ind, longest_last_ind);
    semilogy(1:last_ind, objs_quadratic[1:last_ind, test_ind], "k-",
             linewidth=1);
  end
  axis([1, longest_last_ind, 1e-6, 1e1]);
  savefig("quadratic-pr-random-init.eps", bbox_inches="tight");

  println("\t ########## Good initializations ########## \n\n");
  ## Now, do an experiment with good initializations.
  objs_quadratic = zeros(maxiter_quad + 1, num_tests);
  objs_lone = zeros(maxiter_lone + 1, num_tests);
  srand(0);
  for test_ind = 1:num_tests
    (A, b, xopt) = GeneratePhaseRetrievalData(m, n);
    x_init = xopt + randn(n) / 1.4;
    println("\t*** Lone Objective (", test_ind, " of ", num_tests, ") ***\n");
    (x_lone, objs) =
      LoneProxLinear(A, b, x_init = x_init, maxiter = maxiter_lone);
    objs_lone[1:length(objs), test_ind] = objs;
    println("\t*** Quadratic Objective (", test_ind, " of ",
            num_tests, ") ***\n");
    (x_quad, objs) =
      QuadraticProxLinear(A, b, x_init = x_init, maxiter = maxiter_quad);
    objs_quadratic[1:length(objs), test_ind] = objs;
  end
  # Now plot L1 results
  figure();
  longest_last_ind = 0;
  for test_ind = 1:num_tests
    last_ind = findlast(objs_lone[:, test_ind] .> 0);
    longest_last_ind = max(longest_last_ind, last_ind);
    semilogy(1:last_ind, objs_lone[1:last_ind, test_ind], "k-",
             linewidth=1);
  end
  axis([1, longest_last_ind, 1e-5, 1e3]);
  savefig("l1-pr-good-init.eps", bbox_inches="tight");
  # Plot L2 results
  figure();
  longest_last_ind = 0;
  for test_ind = 1:num_tests
    last_ind = findlast(objs_quadratic[:, test_ind] .> 0);
    longest_last_ind = max(last_ind, longest_last_ind);
    semilogy(1:last_ind, objs_quadratic[1:last_ind, test_ind], "k-",
             linewidth=1);
  end
  axis([1, longest_last_ind, 1e-7, 1e1]);
  savefig("quadratic-pr-good-init.eps", bbox_inches="tight");
end

# (A, b, x) = GeneratePhaseRetrievalData(m, n)
#
# Generates phase retrieval data of size m-by-n.  Sets A to be an
# m-by-n matrix of random Gaussians, and with x a random Gaussian, generates
#
#   b = (A * x).^2
#
# and returns the triple (A, b, x).
function GeneratePhaseRetrievalData(m::Int64, n::Int64)
  A = randn(m, n);
  for ii = 1:m
    A[ii, :] = A[ii, :] / norm(A[ii, :]);
  end
  x = randn(n);
  b = (A * x).^2;
  return (A, b, x);
end

# (x, objs) =
#   LoneProxLinear(A, b; x_init, eps_acc, maxiter, verbose_level)
#
# Performs a prox-linear method on the objective
#
#   ||(Ax)^2 - b||_1
#
# with a small line.  Iteratively sets delta to minimize
#
#   ||(Ax)^2 + 2 * (A * x).* A * delta - b||_1 + delta' * A'* A * delta,
#
# which is guaranteed to make progress on the objective.
function LoneProxLinear(A::Matrix{Float64}, b::Vector{Float64};
                        x_init::Vector{Float64} = randn(size(A, 2)),
                        eps_acc::Float64 = 1e-4,
                        maxiter::Int64 = 2 * size(A, 1),
                        verbose_level::Int64 = 1)
  AtA = A'*A;
  (m, n) = size(A);
  Ax = A * x_init;
  x = copy(x_init);
  obj = norm((Ax).^2 - b, 1);
  objs = zeros(maxiter + 1);
  objs[1] = obj;
  iter = 1;
  steplength = Inf;
  while (steplength > eps_acc && iter < maxiter)
    next_obj = Inf;
    delta_cvx = Variable(n);
    problem = minimize(norm((Ax).^2 + 2 * Ax .* (A * delta_cvx) - b,
                            1) + quadform(delta_cvx, AtA));
    solve!(problem, ECOSSolver(verbose=false));
    delta = delta_cvx.value;
    steplength = norm(delta);
    # Approximate step length by "progress" guarantee in subgradient method
    x = x + delta;
    Ax = A * x;
    obj = norm((Ax).^2 - b);
    iter += 1;
    objs[iter] = obj;
    println("[Iteration ", iter-1, "]  Objective: ",
            obj, "  step: ", steplength);
  end
  objs = objs[1:iter];
  return (x, objs);
end

# (x, objs) =
#   QuadraticProxLinear(A, b; x_init, eps_acc, maxiter)
#
# Performs a prox-linear method on the objective
#
#   ||(Ax)^2 - b||^2
#
# with a little line search to make sure we make progress. Iteratively
# sets delta to minimize
#
#   ||(Ax)^2 + 2 * (A * x).* A * delta - b||^2 + ||delta||^2 / alpha
#      = ||(Ax)^2 + 2 * D * A * delta - b||^2 + ||delta||^2 / alpha
#
# for D = diag(A * x), which has solution
#
#   delta = (A' * D^2 * A + I / alpha) \ (A' * D * ((Ax)^2 - b)).
#
# Then updates x = x + delta, and checks if enough progress is made.
# Terminates when delta / alpha is smaller than eps_acc.
function QuadraticProxLinear(A::Matrix{Float64}, b::Vector{Float64};
                             x_init::Vector{Float64} = randn(size(A, 2)),
                             eps_acc::Float64 = 1e-4,
                             maxiter::Int64 = 2 * size(A, 1),
                             verbose_level::Int64 = 1)
  proj_dec_mult = .4;  # Multiplier on projected decrease from delta
  steplength = 2 * eps_acc;
  stepsize = 1.0 / mean(A.^2);
  (m, n) = size(A);
  D = spdiagm(ones(m));
  grad = zeros(n);
  Ax = A * x_init;
  x = copy(x_init);
  Ax_next = Ax;
  delta = zeros(n);
  obj = mean(((Ax).^2 - b).^2) / 2;
  objs = zeros(maxiter + 1);
  objs[1] = obj;
  iter = 1;
  while (steplength > eps_acc && iter < maxiter)
    Ax = A * x;
    next_obj = Inf;
    delta_dot_grad = -Inf;
    grad = 2 * A' * (Ax .* ((Ax).^2 - b)) / m;
    while (next_obj > obj + proj_dec_mult * delta_dot_grad)
      if (verbose_level >= 2)
        println("\t Backtracking; stepsize ", stepsize);
      end
      D.nzval[:] = Ax;
      delta = -(4 * A' * D.^2 * A + eye(n) / stepsize) \
        (2 * A' * D * ((Ax).^2 - b));
      Ax_next = A * (x + delta);
      next_obj = mean(((Ax_next).^2 - b).^2) / 2;
      delta_dot_grad = dot(grad, delta);
      stepsize /= 2.0;
    end
    stepsize *= 2.0;  # Get stepsize back where it was
    steplength = norm(delta) / stepsize;
    # Increment stepsize so we make more progress hopefully.
    stepsize *= 2.0;
    x = x + delta;
    obj = next_obj;
    iter += 1;
    objs[iter] = obj;
    if (verbose_level >= 1)
      println("[Iteration ", iter-1, "]  Objective: ",
              obj, "  step: ", steplength);
    end
  end
  objs = objs[1:iter];
  return (x, objs);
end

end  # module PR
