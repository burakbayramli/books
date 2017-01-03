function [x,u] = quartic_elements(N);
% warning!  This only works for uniformly spaced x

dx = 1/N;
x = 0:dx:1;

K = zeros(N,N);
K(1,1) = 256/(105*dx);
K(1,2) = -124/(105*dx);
for j=2:N-1
  K(j,j-1) = -124/(105*dx);
  K(j,j) = 256/(105*dx);
  K(j,j+1) = -124/(105*dx);
end
K(N,N-1)= -124/(105*dx);
K(N,N)= 124/(105*dx);

% for f(x) = x^6
for j=1:N-1
    x1 = x(j+1);
    F(j) = (16/3465)*dx*(231*x1^6+495*x1^4*dx^2+165*x1^2*dx^4+5*dx^6);
end
x1 = x(N+1);
F(N) = (1/6930)*dx*(80*dx^6+3696*x1^6-6930*x1^5*dx+7920*x1^4*dx^2-5775*x1^3*dx^3+2640*x1^2*dx^4-693*x1*dx^5);

% want to sove U*K = F
% that is,  U = F*inv(K) = F/K
U = F/K;
u(2:N+1) = U;
