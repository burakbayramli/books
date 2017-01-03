
function bb = bestblocks(x, rl)
n = size(x,2);
x1=max(x);
c1 = x'*ones(1,n) < ones(n,1)*x;
c2 = cumsum(c1);
c3 = (c2 == ones(n,1)*diag(c2)') & ~c1;
reallen = sum((c3(1:n-1,:)&c3(2:n,:)).*(rl'*ones(1,n)));
y = reallen.*(x1-max(c3.*(x1- x'*ones(1,n))));
c4 = c3'.*(y'*ones(1,n));
c5 = (c4 == ones(n,1)*max(c4));
best = find(sum(c5'~=c3)==0);
right = max(((1:n)'*ones(1,n)).*c3);
left = right-sum(c3)+1;
bb = unique([left(best)' right(best)' y(best)'],'rows')';