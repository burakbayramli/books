%5.4  interpolatecode.m

x = cos(pi*(0:n)'/n); y = feval(x); q = [.5; (−1).^((1:n)'); .5]; % weights
xx = linspace(-1,1,N)'; numer = zeros(N,1); denom = zeros(N,1);
for j = 1:(n+1)
  diff = xx - x(j); ratio = q(j)./diff;  % find qj/(xx - x_j) for all xx
  numer = numer + ratio*y(j);            % sum q_ky_k/(xx - x_k) to k = j
  denom = denom + ratio;                 % sum q_k/(xx - x_k) to k = j
end                 % numer and denom now include all terms in formula (9)
yy = numer./denom;  % N values yy = p(xx) of the interpolating polynomial
plot(x,y,'.',xx,yy,'-')
