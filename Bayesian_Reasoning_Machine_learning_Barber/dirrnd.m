function r = dirrnd(alpha,n)
%DIRRND Samples from a Dirichlet distribution
% draw n sample distributions from Dir(alpha(1),...,alpha(K))
r=zeros(length(alpha),n);
for k=1:length(alpha)
    r(k,:) = mygamrnd(alpha(k),1,n);
end
r=r./repmat(sum(r,1),length(alpha),1);