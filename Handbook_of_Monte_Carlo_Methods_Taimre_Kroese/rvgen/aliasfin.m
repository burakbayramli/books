%aliasfin.m
p =rand(1,400);p = p/sum(p); %the distribution from which to sample
n = size(p,2);
a = 1:n; %alias values
q = zeros(1,n); % cutoff values
q = n*p
greater = find(q >= 1);
smaller = find(q < 1);

while (~isempty(smaller) && ~isempty(greater))
    i = smaller(1);
    j = greater(1);
    a(i) = j;
    q(j) = q(j) -(1- q(i));
    if (q(j) < 1)
        greater = setdiff(greater,j);
        smaller = union(smaller,j);
    end
    smaller = setdiff(smaller,i);
end
pp = q/n
for i = 1:n
    ind = find(a == i);
    pp(i) = pp(i) + sum((1 - q(ind)))/n;
end
max(abs(pp - p))
N = 10^6; % generate sample of  size N
X = zeros(1,N);
for i = 1:N
    K = ceil(rand*n);
    if (rand > q(K));
        X(i) = a(K);
    else
        X(i) = K;
    end
end

