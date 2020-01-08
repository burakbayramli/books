function [ tv ] = tv(varargin)
% Computes the 2-norm total variation of a list of matrices
    diffs = [];
    [m,n] = size(varargin{1});
    % iterate over list of matrices
    for i=1:length(varargin)
      A=varargin{i};
      % subtract neighboring columns, subtract neighboring rows
      d = reshape([[A(1:end-1,1:end-1) - A(1:end-1,2:end)], ...
                   [A(1:end-1,1:end-1) - A(2:end,1:end-1)]], (m-1)*(n-1),2);
      diffs = [diffs d];
    end
    % take the 2-norm along the 3rd dimension
    % then sum over rows and cols
    tv = sum(norms(diffs,2,2));
end
