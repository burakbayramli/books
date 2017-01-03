function indices=getdimind(dim,vars)
%GETDIMIND return the indices of variables
% indices=getdimind(dim,vars)
n=sum(dim); % total number of dimensions
v=length(dim); % number of variables
c=1;
for i=1:v
    for j=1:dim(i)
        ind{i}(j)=c;
        c=c+1;
    end
end
indices=horzcat(ind{vars});