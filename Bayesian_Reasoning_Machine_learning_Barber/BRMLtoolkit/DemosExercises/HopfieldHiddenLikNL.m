function L = HopfieldHiddenLikNL(v,Amat,Bmat,Cmat,Dmat,h1)
L=0;
h(:,1)=h1;
for t = 1:size(v,2)-1
    at=Dmat*v(:,t) + Cmat*h(:,t);
    h(:,t+1)= 2*sigma(Amat*h(:,t)+Bmat*v(:,t))-1;
    L = L + sum(log(sigma((2*v(:,t+1)-1).*at)));
end