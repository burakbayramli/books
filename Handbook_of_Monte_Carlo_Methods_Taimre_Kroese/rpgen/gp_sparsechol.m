%gp_sparsechol.m
m = 200; d1 = 1; d2 = -0.25; %at most d1/4 in absolute value
nels = m*(5*m-4);
%preallocate memory to form sparse precision matrix
a = zeros(1, nels); b = zeros(1,nels); c = zeros(1,nels);
%compute the links and weights for the precision matrix
k=0;
for i=1:m
    for j=1:m
        A = findneigh(i,j,m);
        nnb = size(A,1);
        for h=1:nnb
            a(k+h)= ij2k(i,j,m);
            b(k+h)= ij2k(A(h,1),A(h,2),m);
            if h==1
                c(k+h) = d1;
            else
                c(k+h) = d2;
            end
        end
        k = k+nnb;
    end
end
Lambda = sparse(a,b,c,m^2,m^2); %Construct the precision matrix
D = chol(Lambda,'lower'); %calculate the cholesky matrix
Z = randn(m^2,1);
x = D'\Z;  % generate the Gaussian process
colormap gray, brighten(-0.2)
imagesc(reshape(x,m,m)) % plot the result
