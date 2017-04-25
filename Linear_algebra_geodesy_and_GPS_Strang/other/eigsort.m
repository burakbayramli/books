function [v,d] = eigsort(a)
[v1,d1] = eig(a);
d2 = diag(d1);
[dum,idx] = sort(d2);
v = v1(:,idx);
d = diag(d2(idx));
