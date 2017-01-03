% Simple demo of logistic regression in 2D
% Plot true label using color (red=0, green=1)
% Make size of dot proportional to P(y=1|x)

N = 200; p = 2;
posndx = 1:N/2; negndx = (N/2)+1:N;
X = zeros(N, p);
X(posndx,:) = randn(length(posndx), p);
X(negndx,:) = randn(length(negndx), p) + repmat([2 2], length(negndx), 1);
y = ones(N,1);
y(negndx) = 0;

perm = randperm(N);
trainndx = perm(1:round(0.7*N));
testndx = setdiff(perm, trainndx);

beta1  = logregFit(y(trainndx), X(trainndx,:));
beta2 = logregFit2(y(trainndx), X(trainndx,:));
beta3 = logist2(y(trainndx), X(trainndx,:));

colors = 'rg';
figure(1);clf
for i=1:length(testndx)
  n  = testndx(i);
  hold on
  c = y(n)+1;
  p = sigmoid(X(n,:)*beta1);
  m = max(5, 30*p);
  h=plot(X(n,1), X(n,2), 'o', 'markerfacecolor', colors(c),  'markersize', m);
end
