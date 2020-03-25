function x=resample(n,k)
% Samples the numbers 1,...,n uniformly
% without replacement k times, where k<n;
x = zeros(1,n); S = 0;
while S < k
    x(ceil(n * rand(1,k-S))) = 1;
    S = sum(x);
end
x = find(x > 0);
% destroy sorted structure
x = x(randperm(k));


