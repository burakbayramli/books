%gamble_CE_A.m
N = 10^3; %Run Size
results = zeros(N,1);
k = 10; K = 30; %Initial value and absorbing barrier
p = .3; q = 1 - p; r = q/p; %Actual probabilities

%Tilt the distribution using CE
v = ((K - k)*(p - q)*(1 - r^k)) / ((K-k)*(r^k + 1) ...
     + 2 * K * ((r^k - r^K) / (r^K - 1)))
theta = .5*(log((1+v)*q)-log((1-v)*p));
p_tilde = (p * exp(theta)) / (p * exp(theta) + q * exp(-theta));
q_tilde = 1 - p_tilde;

for i = 1:N
    t = 0;
    sum = k;
    while (sum ~= K) && (sum ~= 0)
        t = t+1;
        U = rand;
        sum = sum + (2*(U < p_tilde) - 1);
    end
    results(i) = exp(-theta * (sum - k) + t * (log(p * exp(theta) ...
                 + q * exp(-theta))) )*(sum == K);
end

ell = ((q/p)^k - 1) / ((q/p)^K - 1) %Actual Probability
ell_hat = mean(results) %Estimated Probability
RE = std(results) / sqrt(N) / ell_hat %Estimated Relative Error
