function [traindata, testdata] = partitionDataset(data, pcTrain)
% Partition a dataset into a training set (of percentage size pcTrain) and test set
% function [traindata, testdata] = partitionDataset(data, pcTrain)

% partition into training and testing set in reproducible way
seed = 0;
rand('state', seed);
randn('state', seed);

Ndata = size(data.X, 1);
perm = randperm(Ndata);
Ntrain = floor(Ndata*pcTrain);
trainndx = perm(1:Ntrain);
testndx = perm(Ntrain+1:end);

traindata.X = data.X(trainndx, :);
traindata.C = data.C(trainndx);
testdata.X = data.X(testndx, :);
testdata.C = data.C(testndx);
