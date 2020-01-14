%Newton method for approximate total variation de-noising
%
% problem data
approx_tv_denoising_data;
D = spdiags([-1*ones(n,1) ones(n,1)], 0:1, n-1, n);
% Newton method
ALPHA = 0.01;
BETA = 0.5;
MAXITERS = 100;
NTTOL = 1e-10;

x = zeros(n,1);
newt_dec = [];

for iter = 1:MAXITERS
  d = (D*x);
  val = (x-xcor)'*(x-xcor) + ...
	MU*sum(sqrt(EPSILON^2+d.^2)-EPSILON*ones(n-1,1));
  grad = 2*(x - xcor) + ...
	 MU*D'*(d./sqrt(EPSILON^2+d.^2));
  hess = 2*speye(n) + ...
	 MU*D'*spdiags(EPSILON^2*(EPSILON^2+d.^2).^(-3/2),0,n-1,n-1)*D;
  v = -hess\grad;
  lambdasqr = -grad'*v; newt_dec = [newt_dec sqrt(lambdasqr)];
  if (lambdasqr/2) < NTTOL, break; end;
  t = 1;
  while ((x+t*v-xcor)'*(x+t*v-xcor) + ...
	 MU*sum(sqrt(EPSILON^2+(D*(x+t*v)).^2)-EPSILON*ones(n-1,1)) >
	 val - ALPHA*t*lambdasqr )
    t = BETA*t;
end;
x = x+t*v;
end;

save ("/tmp/xcor.mat", "xcor", "-v7")
save ("/tmp/x.mat", "x", "-v7")
%x
%xcor


