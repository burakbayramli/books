%7.4  conjgradcode.m

n = 10;                     % choose number of iterations
A = diag([1 2 3 4]); b = [1 1 1 1]'; % example of A and b
d0 = b; r0 = b; x0 = 0;     % set initial d,r,x
for k = 1:n
  a = (r0'*r0)/(d0'*A*d0);  % step length to next x_k
  x = x0 + a*d0;            % approximate solution
  r = r0 - a*A*d0;          % new residual from (14)
  b = (r'*r)/(r0'*r0);      % improvement this step
  d = r + b*d0;             % next search direction
  d0 = d; r0 = r; x0 = x;   % update for next iteration
end
d,r,x
