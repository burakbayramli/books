function d=descendents(x,A)
%DESCENDENTS Return the descendents of nodes x in DAG A
% d=descendents(x,A)
% see also ancestors.m
N=size(A,1);
AA=A+eye(N);
AD = real(AA^N>0)-eye(N);
d=[];
for n=x
	d=unique([d find(AD(n,:))]);
end
d=setdiff(d,x);