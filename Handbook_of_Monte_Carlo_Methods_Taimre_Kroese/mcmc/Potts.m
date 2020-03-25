% Potts.m
set(0,'RecursionLimit',100000) % used by clust.m
n = 20; % size of square lattice
N = 40; %number of Gibbs steps
nels =n*(5*n-4); % number of ones in psi_ij
beta =.8 ;   %
a = zeros(1,nels); % preallocation of memory
b = zeros(1,nels);
c = zeros(1,nels);
k=0;
%set up sparse Psi matrix
for i=1:n
    for j=1:n
        A = findneigh(i,j,n); %index of the neighbors of site (i,j)
        number_of_neigh = size(A,1);
        for h=1:number_of_neigh  % convert (i,j) to a linear index
            a(k+h)=sub2ind([n;n],j,i);
            b(k+h)=sub2ind([n;n],A(h,2),A(h,1));
            c(k+h) = 1;
        end
        k = k+number_of_neigh;
    end
end
Psi = sparse(a,b,c,n^2,n^2); % build adjacency matrix
K=3; % number of colors;
x = ceil(rand(1,n^2)*K);  %initial state
for iter=1:N
    iter
    %step 1 of S-W
    B=sparse([],[],[],n^2,n^2); % allocate for {B_ij}
    for i=1:n^2
        neighbors_of_i = find(Psi(i,:));
        for k= neighbors_of_i
            if i < k
                B(i,k) = (rand  < (1 - exp(-beta))*(x(i)==x(k)));
                B(k,i) = B(i,k);
            end
        end
    end
    
    %step 2 of S-W
    [nclust,C] = mkclust(B);% given B, cluster sites
    lims = [0,find(~C)]; % tells where the zeros are
    csizes = diff(lims)-1; %cluster sizes
    for k=1:nclust
        xc = ceil(rand*K); % sample colors uniformly
        for l=(lims(k)+1):(lims(k)+csizes(k))
            x(C(l)) = xc; % assign color xc to  the remaining sites
        end
    end
    imagesc(reshape(x-1,n,n)) % plot the lattice and state of X
    colormap gray
    pause(.01)
end

