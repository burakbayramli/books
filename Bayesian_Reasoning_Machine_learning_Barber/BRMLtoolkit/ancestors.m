function a=ancestors(x,A)
%ANCESTORS Return the ancestors of nodes x in DAG A
% a=ancestors(x,A)
% eg a = ancestors([1 2],A)
N=size(A,1);
AA=A+eye(N); % include self transitions
AD = real(AA^N>0)-eye(N); % after N updates, these are the descendants (exclusing self-self paths)
a=[];
for n=x
	a=unique([a find(AD(:,n))']); % ancestors are those whose descendants are in x
end
a=setdiff(a,x); 