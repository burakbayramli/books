function c = children(A,x)
%CHILDREN return the children of variable x given adjacency matrix A
% c = children(A,x)
c=[]; for i=x(:)'; c = [c find(A(i,:))]; end; c=unique(c);