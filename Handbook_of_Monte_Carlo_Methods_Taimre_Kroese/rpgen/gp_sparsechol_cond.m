clear all
m = 200; % Grid is m x m
d1 = 1; % Central stencil weight
d2 = -0.25; % Neighbour weight

% Elements conditioned on and their values
cond=[ones(m,1),(1:1:m)'];
condk=zeros(size(cond,1),1);

for i=1:size(cond,1)
    condk(i)=ij2k(cond(i,1),cond(i,2),m);
end

yk=20.*ones(m,1);
nels = m*(5*m-4); % Number of non-zero elements on grid

% Pre-allocate memory to form the sparse precision matrix

a = zeros(1,nels);
b = zeros(1,nels);
c = zeros(1,nels);

% Compute the links and weights for precision matrix
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

% Construct the precision matrix
D = sparse(a,b,c,m^2,m^2);
% Construct mean vector

mu=sparse([],[],[],m^2,1,0);
% Obtain remaining indices
idx=(1:1:m^2)';
idx(condk)=[];
% Conditional Precision matrix
Qcond = D(idx,idx);

% Conditional mean vector
mucond=mu(idx)+(Qcond\(-D(idx,condk)*(yk-mu(condk)))); 
C = chol(Qcond,'lower'); % Factor it for generation
z = randn(m^2-length(condk),1); % z is N(0,I)
x = mucond+C'\z; % x is N(mucond,Qcond^{-1}) distributed

% Put it back together

y=zeros(m^2,1);
y(idx)=x;
y(condk)=yk;

% Same for the mean
mm=zeros(m^2,1);
mm(idx)=mucond;
mm(condk)=yk;

% Display some pictures
% Overall
figure,
colormap gray
brighten(-0.2)
imagesc(reshape(y,m,m))

% Mean
figure,
colormap gray
brighten(-0.2)
imagesc(reshape(mm,m,m))

% Conditioned Process less Mean
figure,
colormap gray
brighten(-0.2)
imagesc(reshape(y-mm,m,m))
