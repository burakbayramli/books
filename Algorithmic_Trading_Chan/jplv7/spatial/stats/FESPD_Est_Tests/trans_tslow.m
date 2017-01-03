function results=trans_tslow(x,n)
%PURPOSE : transform the data organisation from i being slow moving index
%and t fast moving index to i being fast moving index and t slow moving
%index.

%x : variable to be transformed (can be multivariate)
%n : number of individuals

nt = length(x);
t = nt/n;
res = [];
for j = 1:t
    for i=1:n
        j2=t*(i-1)+j;
        res = [res; x(j2,:)];
    end;
end;
results=res;