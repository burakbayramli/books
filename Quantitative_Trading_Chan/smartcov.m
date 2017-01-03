function y = smartcov(x)
% Covariance n of finite elements.
% Rows of observations, columns of variables
%   Same as COV except that it ignores NaN and Inf instead of 
%   propagating them
%
%   Normalizes by N, not N-1

y=NaN(size(x, 2), size(x, 2));
xc=NaN(size(x));

goodstk=find(~all(isnan(x), 1));

xc(:, goodstk)=x(:, goodstk)-repmat(smartmean(x(:, goodstk),1), [size(x, 1) 1]);  % Remove mean

for m=1:length(goodstk)
    for n=m:length(goodstk)
        y(goodstk(m), goodstk(n))=smartmean(xc(:, goodstk(m)).*xc(:, goodstk(n)), 1);
        y(goodstk(n), goodstk(m))=y(goodstk(m), goodstk(n));
    end
end