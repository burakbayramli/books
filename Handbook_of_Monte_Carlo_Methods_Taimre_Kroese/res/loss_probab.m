%loss_probab.m
clear all
clc
% set up parameters
n=10^3; gamma=4*10^4;m=21;
k=1:n;
p=0.01*(1+sin(16*pi*k/n));
x=norminv(1-p);
c=1:99/(n-1):100;
% set up matrix for factor design
R=ones(n,1)*0.8;
G=zeros(100,10);
for j=1:10
    G(1+(j-1)*10:10*j,j)=0.4;
end
FF=zeros(1000,10);
for j=1:10
    FF(1+(j-1)*100:100*j,j)=0.4;
end
A=[R,FF,repmat(G,10,1)];
a=sqrt(1-sum(A.^2,2));
% finish setting up matrix for factor design

% run the Hit-and-Run sampler
Y=randn(1,m+n)+4; % find a starting point
mu=0;mu2=0;N=10^5;
for i=1:N
    d=randn(1,m+n); d=d/norm(d);
    lam=-d*Y'+randn;
    Y_new=Y+lam*d; % make proposal
    if score(Y_new(1:m),Y_new(m+1:m+n),x,c,A,a)>gamma
        Y=Y_new;  % accept or reject proposal
    end
    mu=mu+Y/N; % estimate CE parameters
    mu2=mu2+Y.^2/N;
end
sig=sqrt(mu2-mu.^2);
stem(mu(1:m))
% identify the important changes of measure
p_value=1-normcdf(abs(mu)./sig);
idx=find(p_value<0.05);

%now apply importance sampling
N1=10^5;
W=zeros(N1,1); % set up likelihood ratio
for i=1:N1
    Y=randn(1,m+n); Y(idx)=Y(idx).*sig(idx)+mu(idx);
    if score(Y(1:m),Y(m+1:m+n),x,c,A,a)>gamma
        W(i)=prod(sig(idx))*...
            exp(sum((Y(idx)-mu(idx)).^2/2./sig(idx).^2-Y(idx).^2/2));
    end
end
ell=mean(W)
std(W)/sqrt(N1)/ell
Reduction_factor=(ell*(1-ell))/var(W)

