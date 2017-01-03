figure(1); clf
xs = 0:0.01:2;
a = [0.01 0.1 1 4]
b = [0.01 0.1 1 6]

a = [0 0.01 1 5]
b = [0 0.01 1 1]

for i=1:4
  p = gampdf(xs, a(i), 1/b(i));
  p2 = gamma_pdf(xs, a(i), b(i));
  subplot(2,2,i)
  plot(xs,p2, 'b-', 'linewidth', 2);
  %hold on; plot(xs,p2+eps*randn(size(p2)),  'r-');
  title(sprintf('a=%4.2f, b=%4.2f', a(i), b(i)))
end


% Inverse gamma
figure(2);clf;
xs = 0:0.01:2;
a = [0 0.01 1 5]
b = [0 0.01 1 1]
for i=1:4
  p = inverse_gamma_pdf(xs, a(i), b(i));
  subplot(2,2,i)
  plot(xs,p, 'linewidth', 2);
  title(sprintf('a=%4.2f, b=%4.2f', a(i), b(i)))
end

