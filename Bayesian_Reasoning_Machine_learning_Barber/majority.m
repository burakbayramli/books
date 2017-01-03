function m=majority(x)
%MAJORITY Return majority values in each column on a matrix
% m=majority(x)
% return the majority value in each column of x. If there is a tie the lowest value is returned
for i=1:size(x,2);
    u=unique(x(:,i)); s=zeros(1,length(u));
    for j=1:length(u)
        s(j)=length(find(x(:,i)==j));
    end
    [val ind]=max(s);
    m(i)=u(ind);
end