xs = 0:0.01:1;
a = [0.1 1];
b = [0.1 1];
for i=1:2
  p = betapdf(xs, a(i), b(i));
  subplot(1,2,i)
  plot(xs,p, 'linewidth', 2);
  title(sprintf('a=%4.2f, b=%4.2f', a(i), b(i)))
end


xs = 0:0.01:1;
a = [0.1 1 2 8];
b = [0.1 1 3 4];
for i=1:4
  p = betapdf(xs, a(i), b(i));
  subplot(2,2,i)
  plot(xs,p, 'linewidth', 2);
  title(sprintf('a=%4.2f, b=%4.2f', a(i), b(i)))
end


folder = 'C:/kmurphy/figures/other';
thetas = 0:0.01:1;
alpha1 = 2; alpha0 = 2;
N1=1; N0=0; N = N1+N0;
prior = betapdf(thetas, alpha1, alpha0);
lik =  thetas.^N1 .* (1-thetas).^N0;	
post = betapdf(thetas, alpha1+N1, alpha0+N0);
h=figure;
%set(gcf, 'position', [100 100 800 200])
subplot(1,3,1);plot(thetas, prior, 'linewidth', 2); 
axis([0 1 0 2]); title('p(\theta)=Be(2,2)')
subplot(1,3,2);plot(thetas, lik, 'linewidth', 2); 
axis([0 1 0 2]); title('p(x=1|\theta)');
subplot(1,3,3);plot(thetas, post, 'linewidth', 2); 
axis([0 1 0 2]); title('p(\theta|x=1)=Be(3,2)')
print(gcf, '-depsc', fullfile(folder, 'betaUpdateX1.eps'))


thetas = 0:0.01:1;
alpha1 = 3; alpha0 = 2;
N1=1; N0=0; N = N1+N0;
prior = betapdf(thetas, alpha1, alpha0);
lik =  thetas.^N1 .* (1-thetas).^N0;	
post = betapdf(thetas, alpha1+N1, alpha0+N0);
figure;
%set(gcf, 'position', [100 100 800 200])
subplot(1,3,1);plot(thetas, prior, 'linewidth', 2); 
axis([0 1 0 2]); title('p(\theta)=Be(3,2)')
subplot(1,3,2);plot(thetas, lik, 'linewidth', 2); 
axis([0 1 0 2]); title('p(x=1|\theta)');
subplot(1,3,3);plot(thetas, post, 'linewidth', 2); 
axis([0 1 0 2]); title('p(\theta|x=1)=Be(4,2)')
print(gcf, '-depsc', fullfile(folder, 'betaUpdateX11.eps'))


thetas = 0:0.01:1;
alpha1 = 4; alpha0 = 2;
N1=1; N0=0; N = N1+N0;
prior = betapdf(thetas, alpha1, alpha0);
lik =  thetas.^N1 .* (1-thetas).^N0;	
post = betapdf(thetas, alpha1+N1, alpha0+N0);
figure;
%set(gcf, 'position', [100 100 800 200])
subplot(1,3,1);plot(thetas, prior, 'linewidth', 2); 
axis([0 1 0 2]); title('p(\theta)=Be(4,2)')
subplot(1,3,2);plot(thetas, lik, 'linewidth', 2); 
axis([0 1 0 2]); title('p(x=1|\theta)');
subplot(1,3,3);plot(thetas, post, 'linewidth', 2); 
axis([0 1 0 2]); title('p(\theta|x=1)=Be(5,2)')
print(gcf, '-depsc', fullfile(folder, 'betaUpdateX111.eps'))

thetas = 0:0.01:1;
alpha1 = 2; alpha0 = 2;
N1=3; N0=0; N = N1+N0;
prior = betapdf(thetas, alpha1, alpha0);
lik =  thetas.^N1 .* (1-thetas).^N0;	
post = betapdf(thetas, alpha1+N1, alpha0+N0);
figure;
%set(gcf, 'position', [100 100 800 200])
subplot(1,3,1);plot(thetas, prior, 'linewidth', 2); 
axis([0 1 0 2]); title('p(\theta)=Be(2,2)')
subplot(1,3,2);plot(thetas, lik, 'linewidth', 2); 
axis([0 1 0 2]); title('p(D=1,1,1|\theta)');
subplot(1,3,3);plot(thetas, post, 'linewidth', 2); 
axis([0 1 0 2]); title('p(\theta|D=1,1,1)=Be(5,2)')
print(gcf, '-depsc', fullfile(folder, 'betaUpdateX111b.eps'))
