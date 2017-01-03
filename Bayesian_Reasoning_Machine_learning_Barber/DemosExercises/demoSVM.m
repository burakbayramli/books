function demoSVM
%DEMOSVM Demo to train an SVM
% Written by Zakria Hussain, UCL, 2009
figure
% Create some nonlinearly separable artificial data
Xpos= repmat([1 -1],10,1)+0.4*randn(10,2);
Xpos= vertcat(Xpos, repmat([-1 1],10,1)+0.4*randn(10,2));
Xneg = repmat([-1 -1],10,1)+0.4*randn(10,2);
Xneg= vertcat(Xneg, repmat([1 1],10,1)+0.4*randn(10,2));
Xtrain = [Xpos; Xneg];
Ytrain = [ones(20,1); -ones(20,1)]; % create labels

plot(Xpos(:,1),Xpos(:,2),'ro','markersize',10,'markerfacecolor','r');
hold on;
plot(Xneg(:,1),Xneg(:,2),'bo','markersize',10,'markerfacecolor','b');

% Create a kernel (Gaussian)
sigma = 1; % width parameter (Note: can tune this with cross-validation)
m = size(Xtrain,1);  % number of examples 
D = sum(Xtrain.^2,2)*ones(1,m) + ones(m,1)*sum(Xtrain.^2,2)' - 2*(Xtrain*Xtrain'); % calculate L2 metric
trainkernel = exp(-D/(2*sigma^2));

% Set up for SMO algorithm
Q = repmat(Ytrain,1,m).*trainkernel.*repmat(Ytrain',m,1); % Q_{ij} = y_i y_j k_{ij}

% Train the SVM
C = 100; % error parameter (Note: can tune this with cross-validation) ...
% -- smaller C allows more misclassifications during training
alpha = SVMtrain(Q,Ytrain,C);  % train SVM to find the dual wight vector
svs = find(alpha); % find support vectors
plot(Xtrain(svs,1),Xtrain(svs,2),'go','markersize',15,'linewidth',2);

% Calculate training error
ftrain = trainkernel(:,svs)*(Ytrain(svs).*alpha(svs));  % decision value
prediction = sign(ftrain);  % prediction on training set
train_err = sum(prediction.*Ytrain<0)/m

% Generate test data
Xp= repmat([1 -1],10,1)+0.4*randn(10,2);
Xp= vertcat(Xp, repmat([-1 1],10,1)+0.4*randn(10,2));
Xn= repmat([-1 -1],10,1)+0.4*randn(10,2);
Xn= vertcat(Xn, repmat([1 1],10,1)+0.4*randn(10,2));

Xtest = [Xp; Xn];
Ytest = [ones(20,1); -ones(20,1)]; % create labels

% Calculate test kernel (Gaussian)
Xsv = Xtrain(svs,:);
msv = size(Xsv,1);  % number of support vectors
n = size(Xtest,1); % number of test examples
D = sum(Xtest.^2,2)*ones(1,msv) + ones(n,1)*sum(Xsv.^2,2)' - 2*(Xtest*Xsv'); % calculate L2 metric
testkernel = exp(-D/(2*sigma^2));

% Calculate test error
ftest = testkernel*(Ytest(svs).*alpha(svs));  % decision value
prediction = sign(ftest);  % prediction on test set
test_err = sum(prediction.*Ytest<0)/m
ptest = find(prediction>0); ntest = find(prediction<0);
plot(Xtest(ptest,1),Xtest(ptest,2),'ro','markersize',10);
plot(Xtest(ntest,1),Xtest(ntest,2),'bo','markersize',10);