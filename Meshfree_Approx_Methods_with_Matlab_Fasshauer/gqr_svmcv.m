function residual = gqr_svmcv(cv_fold,x,y,ep,bc,low_rank)
% Perform a cross-validation on the data to create an objective
% function to optimize

switch nargin
    case 5
        low_rank = 0;
    case 6
    otherwise
        error('Unacceptable arguments passed, nargin=%d',nargin)
end

% Hold on to existing data to preserve the cross-validation
persistent x_old cv_fold_old train_index valid_index

if isempty(x_old) || any(size(x_old)~=size(x)) || any(any(x_old~=x)) || cv_fold~=cv_fold_old
    N = length(y);
    cv_index_list = randperm(N);
    cv_size = floor(N/cv_fold);
    train_index = cell(cv_fold);
    valid_index = cell(cv_fold);

    % Find the folds in the cross-validation
    k_cv = 1;
    for k=1:cv_fold-1
        train_index{k} = sort(cv_index_list(k_cv:k_cv+cv_size-1));
        valid_index{k} = setdiff(1:N,train_index{k});
        k_cv = k_cv + cv_size;
    end
    train_index{cv_fold} = sort(cv_index_list(k_cv:end));
    valid_index{cv_fold} = setdiff(1:N,train_index{cv_fold});
    
    cv_fold_old = cv_fold;
    x_old = x;
end

residual = 0;
for k=1:cv_fold
    train_data = x(train_index{k},:);
    train_class = y(train_index{k});
    valid_data = x(valid_index{k},:);
    valid_class = y(valid_index{k});
    SVM = gqr_fitsvm(train_data,train_class,ep,bc,low_rank);
    residual = residual + errcompute(SVM.eval(valid_data),valid_class);
end