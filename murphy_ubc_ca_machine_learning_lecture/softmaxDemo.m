% Simple demo of multinomial logistic regression
% 3 classes in 2D

N = 100; p = 2;
classndx{1} = 1:30; classndx{2} = 31:60; classndx{3} = 61:100;
K = 3; % 3 classes
X = zeros(p,N);
mu = [-1 1; 0 0; 1 1]'; % rand(p,K);
X = rand(p,N);
Y = zeros(K,N);
for k=1:K
  X(:, classndx{k}) = mvnrnd(mu(:,k)', 0.1*eye(p), length(classndx{k}))';
  Y(k, classndx{k}) = 1;
end

perm = randperm(N);
trainndx = perm(1:80);
testndx = setdiff(perm, trainndx);

beta1  = softmaxFit(Y(:,trainndx), X(:,trainndx));
beta2  = logistK(Y(:,trainndx), X(:,trainndx));

post1 = softmaxApply(beta1, X(:,testndx));

