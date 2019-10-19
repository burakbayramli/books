function p = pfun(z,alpha)

p = zeros(size(z));
y = -alpha*z;
for i=1:length(y)
  if (y(i) < 1e2)
    p(i) = z(i) + log(1+exp(y(i)))/alpha;
  end
end
return;
