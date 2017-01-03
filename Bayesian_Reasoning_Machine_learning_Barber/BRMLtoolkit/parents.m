function p = parents(A,x)
%PARENTS return the parents of variable x given adjacency matrix A
% p = parents(A,x)
p=[]; for i=x(:)'; t=find(A(:,i)); p = [p t(:)']; end; p=unique(p);