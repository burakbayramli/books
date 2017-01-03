% gibbsDemoDenoising
% Denoising of letter A using Gibbs sampling
% with an Ising Prior and a Gaussian likelihood
% Based on code originally written by Brani Vidakovic

seed = 3;
randn('state',seed)
rand ('state',seed)

sigma = 2; % noise level

% input matrix consisting of letter A. The body of letter
% A is made of 1's while the background is made of -1's.
img = imread('lettera.bmp'); 
[M,N] = size(img);
img = double(img);
m = mean(img(:));
img2 = +1*(img>m) + -1*(img<m); % -1 or +1
y = img2 + sigma*randn(size(img2)); %y = noise signal


% observation model
offState = 1; onState = 2;
mus = zeros(1,2);
mus(offState) = -1; mus(onState) = +1;
sigmas = [sigma sigma];
Npixels = M*N;
localEvidence = zeros(Npixels, 2);
for k=1:2
  localEvidence(:,k) = normpdf(y(:), mus(k), sigmas(k));
end

[junk, guess] = max(localEvidence, [], 2);  % start with best local guess
X = ones(M, N);
X(find(guess==offState)) = -1;
X(find(guess==onState)) = +1;
Xinit = X;

doPrint = 0;

figure;
imagesc(y);colormap gray; axis square; axis off
title(sprintf('sigma=%2.1f', sigma))
fname = sprintf('figures/gibbsDemoDenoisingOrigS%2.1f.eps', sigma);
if doPrint, print(gcf, '-depsc', fname); end

figure;
imagesc(Xinit);colormap gray; axis square; axis off
title('initial guess')
fname = sprintf('figures/gibbsDemoDenoisingInitS%2.1f.eps', sigma);
if doPrint, print(gcf, '-depsc', fname); end

fig = figure; clf
pause

J = 1;
avgX = zeros(M,N);
X = Xinit;
maxIter = 100000;
for iter =1:maxIter
  % select a pixel at random
  ix = ceil( N * rand(1) ); iy = ceil( M * rand(1) );
  pos = iy + M*(ix-1);
  neighborhood = pos + [-1,1,-M,M];  
  neighborhood(find([iy==1,iy==M,ix==1,ix==N])) = [];
  % compute local conditional
  wi = sum( X(neighborhood) );
  p1  = exp(J*wi) * localEvidence(pos,onState);
  p0  = exp(-J*wi) * localEvidence(pos,offState);
  prob = p1/(p0+p1);
  if rand < prob
    X(pos) = +1;
  else
    X(pos) = -1;
  end
  avgX = avgX+X;
  % plotting
  if rem(iter,10000) == 0,
    figure(fig);
    imagesc(X);  axis('square'); colormap gray; axis off;
    title(sprintf('sample %d', iter));
    drawnow
  end
  if doPrint % iter==10000 | iter==50000 | iter==100000
    figure;
    imagesc(X);colormap gray; axis square; axis off
    title(sprintf('sample %d', iter))
    fname = sprintf('figures/gibbsDemoDenoisingIter%dJ%3.2fS%2.1f.eps', iter, J, sigma);
    print(gcf, '-depsc', fname);
  end
end

figure;
imagesc(avgX);colormap gray; axis square; axis off
title(sprintf('posterior mean after %d samples', iter))
fname = sprintf('figures/gibbsDemoDenoisingMean%dJ%3.2fS%2.1f.eps', iter, J, sigma);
if doPrint, print(gcf, '-depsc', fname); end
