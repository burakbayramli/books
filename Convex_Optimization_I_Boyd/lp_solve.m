function [x_star,p_star,gap,status,nsteps] = lp_solve(A,b,c);
  [m,n] = size(A);
  nsteps = zeros(2,1);
  x0 = A\b; t0 = 2+max(0,-min(x0));
  if min(x0)>0.0
    disp('A\b already feasible')
    x_0 = x0
  else
    disp('phase I')
    A1 = [A,-A*ones(n,1)];
    b1 = b-A*ones(n,1);
    z0 = x0+t0*ones(n,1)-ones(n,1);
    c1 = [zeros(n,1);1];
    [z_star, history, gap] = lp_barrier(A1,b1,c1,[z0;t0]);

    if (z_star(n+1) >= 1)
      fprintf('\nProblem is infeasible\n');
      x_star = []; p_star = Inf; status = 'Infeasible';
      nsteps(1) = sum(history(1,:)); gap = [];
      return;
    end
    fprintf('\nFeasible point found\n');
    nsteps(1) = sum(history(1,:));
    x_0 = z_star(1:n)-z_star(n+1)*ones(n,1)+ones(n,1);
  end
    
  [x_star, history, gap] = lp_barrier(A,b,c,x_0);
  status = 'Solved'; p_star = c'*x_star;
  nsteps(2) = sum(history(1,:));

  
  
