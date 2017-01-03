function y = nearNeigh(xtrain, xtest, trainlabels, K)
%NEARNEIGH Nearest Neighbour classification
% y=nearNeigh(xtrain, xtest, trainlabels,K)
% calculate the nearest  neighbour classification (use squared distance to measure dissimilarity)
% xtrain : matrix with each column a training vector
% xtest : matrix with each column a test vector
% trainlabels : vector of length size(xtrain,2) of training labels
ntrain = size(xtrain,2); % number of training points
ntest = size(xtest,2); % number of test points
[vals, ind] = sort(sqdist(xtrain,xtest));
if K==1
    y =trainlabels(ind(1,:));
else
    if size(xtest,2)>1
        y = majority(trainlabels(ind(1:K,:)));
    else
        y = majority(trainlabels(ind(1:K,:))');
    end
end