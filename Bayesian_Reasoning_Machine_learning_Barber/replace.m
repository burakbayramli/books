function Y=replace(X,f,r)
%REPLACE Replace instances of a value with another value
% X=replace(X,f,r)
% replace instances of f by r in matrix X
Y=X;
for k=1:length(f)
    if isnan(f(k))
        Y(isnan(X))=r(k);
    elseif isinf(f(k))
        Y(isinf(X))=r(k);
    else
        Y(X==f(k))=r(k);
    end
end