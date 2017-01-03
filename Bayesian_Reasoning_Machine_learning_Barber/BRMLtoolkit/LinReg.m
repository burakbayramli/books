function [W mse]=LinReg(X,Y,lambda)
% [W mse]=LinReg(X,Y,lambda)
% Linear Regression: Y=W*X
%
% The routine returns the least squares solution to
% sum(sum((Y-W*X).^2))+ N*sum_j lambda(j) W(:,j).^2
% where N is the number of datapoints
% lambda is a vector of regularisation constants.
%
% inputs:
% Each column of Y is a datapoint
% Each column of X is a datapoint
%
% outputs:
% W and the mean square error 
M=X*X'-diag(lambda);
B=Y*X';
W=B/M;
Z=Y-W*X;
mse=mean(Z(:).^2);