% function u = linear_elements(@f,x)
function u = linear_elements(f,x);

N = length(x)-1;
dx = diff(x);

K = zeros(N,N);

K(1,1) = 1/dx(1) + 1/dx(2);
K(1,2) = -1/dx(2);
for j=2:N-1
  K(j,j-1) = -1/dx(j);
  K(j,j) = 1/dx(j) + 1/dx(j+1);
  K(j,j+1) = -1/dx(j+1);
end
K(N,N-1)= -1/dx(N);
K(N,N)= 1/dx(N);


F = makeF(f,x,dx);
% want to sove U*K = F
% that is,  U = F*inv(K) = F/K
U = F/K;
u(2:N+1) = U;
