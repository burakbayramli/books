%3.1  FDMcode.m

n=4; h=1/(n+1); x=(1:n)'*h; f=2*ones(n,1)-x;   % f(x)=2-x at n interior nodes
mid=(.5:(n+.5))'*h; c=2*ones(n+1,1)-mid; C=diag(c);  % c(x)=2-x at n+1 midpts
A=toeplitz([1 -1 zeros(1,n-1)],[1 zeros(1,n-1)]); % n+1 by n back differences
K=A'*C*A/h^2;         % stiffness matrix has c_left + c_right on its diagonal
U=K\f                      % U_0 = 0 and U_(n+1)=0 to match u(0)=0 and u(1)=0
uexact=(-f.^2 + ones(n,1) + 3*log(f)/log(2))/4;    error = uexact-U
