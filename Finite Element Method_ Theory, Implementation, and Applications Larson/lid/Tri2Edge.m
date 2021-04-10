function edges = Tri2Edge(p,t)
np=size(p,2); % number of vertices
nt=size(t,2); % number of triangles
i=t(1,:); % i=1st vertex within all elements
j=t(2,:); % j=2nd
k=t(3,:); % k=3rd
A=sparse(j,k,-1,np,np); % 1st edge is between (j,k)
A=A+sparse(i,k,-1,np,np); % 2nd (i,k)
A=A+sparse(i,j,-1,np,np); % 3rd (i,j)
A=-((A+A.')<0);
A=triu(A); % extract upper triangle of A
[r,c,v]=find(A); % rows, columns, and values(=-1)
v=[1:length(v)]; % renumber values (ie. edges)
A=sparse(r,c,v,np,np); % reassemble A
A=A+A'; % expand A to a symmetric matrix
edges=zeros(nt,3);
for k=1:nt
edges(k,:)=[A(t(2,k),t(3,k))
A(t(1,k),t(3,k))
A(t(1,k),t(2,k))]';
end
