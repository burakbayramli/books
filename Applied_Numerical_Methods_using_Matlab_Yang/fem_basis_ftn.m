function p=fem_basis_ftn(N,S)
%p(i,s,1:3): coefficients of each basis ftn phi_i 
%               for s-th subregion(triangle)
%N(n,1:2) : x & y coordinates of the n-th node
%S(s,1:3) : the node #s of the s-th subregion(triangle)
N_n= size(N,1); % the total number of nodes
N_s= size(S,1); % the total number of subregions(triangles)
for n=1:N_n
  for s=1:N_s
    for i=1:3
      A(i,1:3)= [1 N(S(s,i),1:2)];
      b(i)= (S(s,i)==n); %The nth basis ftn is 1 only at node n.
    end
pnt=A\b'; 
    for i=1:3, p(n,s,i)=pnt(i); end
  end
end
