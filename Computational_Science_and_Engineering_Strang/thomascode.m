%1.3  thomascode.m

for i=1:n-1,
  c(i) = c(i)/b(i);
  f(i) = f(i)/b(i);
  b(i+1) = b(i+1) - a(i+1)*c(i);
  f(i+1) = f(i+1) - a(i+1)*f(i);
end %forward loop
  u(n) = f(n)/b(n);
for i=n-1:1,
  u(i) = f(i) - c(i)*u(i+1);
end %backward loop
